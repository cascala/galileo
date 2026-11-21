package galileo.geometry

import galileo.environment.Environment
import galileo.expr._
import galileo.linalg.{DenseMatrix, Matrix}
import galileo.plot.Displayable
import galileo.tensor._

import TensorIndexKind._

// Helpers to read Galileo language arguments
object Args {
  // [a b c] or [a; b; c] as a flat list
  def list( e: Expr ): Option[ List[ Expr ] ] = e match {
    case m: Matrix => Some( m.toDenseMatrix.rows.flatten )
    case other => None
  }

  def variables( e: Expr ): Option[ List[ Variable ] ] = list( e ).flatMap { es =>
    val vs = es.collect { case v: Variable => v }
    if ( vs.size == es.size ) Some( vs ) else None
  }

  // Evaluate a symbolic argument without substituting variable values (so that a
  // coordinate called r stays symbolic even if r = 1 was set); a bare variable name
  // is looked up, e.g. metric([r phi], m)
  def symbolic( e: Expr, env: Option[ Environment ] ): Expr = e match {
    case v: Variable => v.visit( env )
    case other => other.visit( None )
  }

  def metric( e: Expr, env: Option[ Environment ] ): Either[ String, Metric ] = e.visit( env ) match {
    case m: Metric => Right( m )
    case other => Left( "expected a metric (e.g. g = metric([r phi], [1 0; 0 r^2])), got " + other )
  }
}

// metric( [x1 x2 ...], [g11 g12 ...; g21 ...] ): a metric g_ab given by its components
case class MetricU( coords: Expr, matrix: Expr ) extends Expr with Statement {
  def info( env: Option[ Environment ] = None ) = "MetricU(" + coords + "," + matrix + ")"
  override def visit( env: Option[ Environment ] = None ): Expr = {
    // coordinates are names: don't substitute values the user may have assigned to them
    val vars = Args.variables( coords ).orElse( Args.variables( coords.visit( env ) ) )
    ( vars, Args.symbolic( matrix, env ) ) match {
      case ( Some( vs ), m: Matrix ) if m.numRows == vs.size && m.numCols == vs.size =>
        Metric( vs, m.toDenseMatrix.rows.flatten, Lower )
      case ( None, _ ) => ErrorExpr( "metric: the first argument must be a row of coordinate names, e.g. [r phi]" )
      case _ => ErrorExpr( "metric: the second argument must be a square matrix with one row per coordinate" )
    }
  }
}

// Ready-made metrics: metric.generate(name)
object MetricTemplates {
  private def v( n: String ) = Variable( n )
  private def n( d: Double ) = Number( d )
  private def diag( vs: List[ String ], d: List[ Expr ] ): Metric = {
    val k = d.size
    Metric( vs.map( v ), ( for ( i <- 0 until k; j <- 0 until k ) yield if ( i == j ) d( i ) else n( 0 ) ).toList, Lower )
  }
  def minkowski = diag( List( "t", "x", "y", "z" ), List( n( -1 ), n( 1 ), n( 1 ), n( 1 ) ) )
  def polar = diag( List( "r", "phi" ), List( n( 1 ), Square( v( "r" ) ) ) )
  def euclidean = diag( List( "x", "y", "z" ), List( n( 1 ), n( 1 ), n( 1 ) ) )
  def spherical = diag( List( "r", "theta", "phi" ), List( n( 1 ), Square( v( "r" ) ), Product( Square( v( "r" ) ), Square( galileo.trigonometry.SinF1( v( "theta" ) ) ) ) ) )
  // torus with tube radius a and central radius R
  def torus = diag( List( "theta", "phi" ), List( Square( v( "a" ) ), Square( Sum( v( "R" ), Product( v( "a" ), galileo.trigonometry.CosF1( v( "theta" ) ) ) ) ) ) )
  // upper half plane model of the hyperbolic plane
  def hyperbolic = diag( List( "x", "y" ), List( Fraction( n( 1 ), Square( v( "y" ) ) ), Fraction( n( 1 ), Square( v( "y" ) ) ) ) )

  val names = List( "minkowski", "polar", "euclidean", "spherical", "two-sphere", "three-sphere", "schwarzschild", "torus", "hyperbolic" )
}

// The metric induced on a parametrised surface (or curve, or hypersurface) by
// the Euclidean metric of the space it is embedded in: g_ij = sum_k dX_k/du_i dX_k/du_j
// (the first fundamental form)
object InducedMetric {
  def apply( embedding: List[ Expr ], coords: List[ Variable ] ): Metric = {
    val jac = coords.map( u => embedding.map( x => Derivative( x, u ).visit() ) )
    val comps = for ( i <- coords.indices; j <- coords.indices ) yield
      Tidy( Sum( jac( i ).zip( jac( j ) ).map { case ( a, b ) => Product( a, b ) }: _* ).visit() )
    Metric( coords, comps.toList, Lower )
  }
}

case class InducedMetricU( embedding: Expr, coords: Expr ) extends Expr with Statement {
  def info( env: Option[ Environment ] = None ) = "InducedMetricU(" + embedding + "," + coords + ")"
  override def visit( env: Option[ Environment ] = None ): Expr =
    ( Args.list( Args.symbolic( embedding, env ) ), Args.variables( coords ) ) match {
      case ( Some( xs ), Some( us ) ) => InducedMetric( xs, us )
      case _ => ErrorExpr( "usage: inducedmetric([x(u,v) y(u,v) z(u,v)], [u v])" )
    }
}

// Geodesic equations  d^2 x^a/ds^2 = - Gamma^a_bc dx^b/ds dx^c/ds
object GeodesicEquations {
  def dot( v: Variable ) = Variable( v.name.trim + "'" )
  def ddot( v: Variable ) = Variable( v.name.trim + "''" )

  def rhs( metric: Metric ): List[ Expr ] = {
    val cv = Curvature( metric )
    val n = metric.dimension
    val xs = metric.variables
    ( 0 until n ).map { a =>
      val terms = for ( b <- 0 until n; c <- 0 until n ) yield {
        val g = cv.gamma( a, b, c )
        g match {
          case Number( 0.0 ) => None
          case _ => Some( Product( Number( -1 ), g, dot( xs( b ) ), dot( xs( c ) ) ) )
        }
      }
      terms.flatten match {
        case Seq() => Number( 0 )
        case many => Tidy( Sum( many: _* ).visit() )
      }
    }.toList
  }
}

// A list of equations lhs = rhs, displayed nicely
case class Equations( title: String, eqs: List[ ( Expr, Expr ) ] ) extends Expr with Statement with Displayable {
  def info( env: Option[ Environment ] = None ) = "Equations(" + title + ")"
  override def toString() = ( title :: eqs.map { case ( l, r ) => "  " + l + " = " + r } ).mkString( "\n" )
  def latex = "$$\\begin{aligned}\n" + eqs.map { case ( l, r ) => Latex( l ) + " &= " + Latex( r ) }.mkString( " \\\\\n" ) + "\n\\end{aligned}$$"
  def mimeBundle = Map( "text/plain" -> toString(), "text/latex" -> latex, "text/markdown" -> ( "**" + title + "**\n\n" + latex ) )
}

// Display of a metric as a line element ds^2 = g_ab dx^a dx^b
object LineElement {
  private def terms( m: Metric ): List[ ( Expr, Variable, Variable ) ] = {
    val n = m.dimension
    ( for ( a <- 0 until n; b <- a until n ) yield {
      val g = Tidy( m.valueAt( a, b ) )
      val c = if ( a == b ) g else Tidy( Product( Number( 2 ), g ).visit() )
      ( c, m.variables( a ), m.variables( b ) )
    } ).filter { case ( c, _, _ ) => c != Number( 0 ) }.toList
  }

  private def nm( v: Variable ) = v.name.trim

  def text( m: Metric ): String = {
    val upper = m.kind == Upper
    val ts = terms( m ).map { case ( c, a, b ) =>
      val d = if ( a == b ) "d" + nm( a ) + "^2" else "d" + nm( a ) + " d" + nm( b )
      c match {
        case Number( 1.0 ) => d
        case _: Sum => "(" + c + ")*" + d
        case _ => c.toString + "*" + d
      }
    }
    val body = if ( ts.isEmpty ) "0" else ts.mkString( " + " ).replace( "+ (-1.0)*", "- " )
    if ( upper ) "Inverse metric g^ab (coordinates " + m.variables.map( nm ).mkString( ", " ) + "): " + components( m )
    else "ds^2 = " + body
  }

  private def components( m: Metric ) = ( for ( a <- 0 until m.dimension ) yield
    ( for ( b <- 0 until m.dimension ) yield Tidy( m.valueAt( a, b ) ).toString ).mkString( "[", ", ", "]" ) ).mkString( "[", ", ", "]" )

  def latex( m: Metric ): String = {
    if ( m.kind == Upper ) {
      val rows = ( 0 until m.dimension ).map( a => ( 0 until m.dimension ).map( b => Latex( Tidy( m.valueAt( a, b ) ) ) ).mkString( " & " ) ).mkString( " \\\\ " )
      return "$$g^{ab} = \\begin{pmatrix} " + rows + " \\end{pmatrix}$$"
    }
    val ts = terms( m ).zipWithIndex.map { case ( ( c, a, b ), i ) =>
      val d = if ( a == b ) "d" + Latex.name( nm( a ) ) + "^2" else "d" + Latex.name( nm( a ) ) + "\\, d" + Latex.name( nm( b ) )
      val neg = Latex( c ).startsWith( "-" )
      val cl = c match {
        case Number( 1.0 ) => ""
        case Number( -1.0 ) => ""
        case _: Sum => "\\left(" + Latex( c ) + "\\right)"
        case _ => if ( neg ) Latex( c ).drop( 1 ) else Latex( c )
      }
      val sign = if ( c == Number( -1.0 ) || ( neg && !c.isInstanceOf[ Sum ] ) ) " - " else if ( i == 0 ) "" else " + "
      sign + cl + "\\, " + d
    }
    "$$ds^2 = " + ( if ( ts.isEmpty ) "0" else ts.mkString.trim ) + "$$"
  }
}

// geometry commands that take a metric
case class MetricFunctionU( name: String, expr: Expr ) extends Expr with Statement {
  def info( env: Option[ Environment ] = None ) = name + "(" + expr + ")"
  override def visit( env: Option[ Environment ] = None ): Expr = Args.metric( expr, env ) match {
    case Left( err ) => ErrorExpr( name + ": " + err )
    case Right( m ) => try name match {
      case "gausscurvature" => Curvature( m ).gaussian
      case "kretschmann" => Curvature( m ).kretschmann
      case "geodesics" => {
        val rhs = GeodesicEquations.rhs( m )
        Equations( "Geodesic equations (' = d/ds along the curve)", m.variables.zip( rhs ).map { case ( x, r ) => ( GeodesicEquations.ddot( x ), r ) } )
      }
      case "inverse" => m.toUpper
      case _ => ErrorExpr( "unknown function " + name )
    } catch {
      case e: IllegalArgumentException => ErrorExpr( name + ": " + Option( e.getMessage ).getOrElse( "invalid argument" ).stripPrefix( "requirement failed: " ) )
    }
  }
}

// Named, simplified results of the curvature commands (christoffel, riemann, ricci, ...)
object Named {
  import Symmetry._
  def christoffel( kind: String, m: Metric ): Components = {
    val c = Curvature( m )
    if ( kind == "ChristoffelFirstU" )
      Components( "Christoffel symbols of the first kind", "Gamma", "\\Gamma", List( Lower, Lower, Lower ), m.variables, c.flat3( c.christoffelFirst ), LastPair )
    else
      Components( "Christoffel symbols of the second kind", "Gamma", "\\Gamma", List( Upper, Lower, Lower ), m.variables, c.christoffelFull.toList, LastPair )
  }

  def tensor( kind: String, m: Metric ): Expr = {
    val cv = Curvature( m )
    def c( title: String, sym: String, kinds: List[ TensorIndexKind ], comps: List[ Expr ], s: Symmetry ) =
      Components( title, sym, sym, kinds, m.variables, comps, s )
    val ll = List( Lower, Lower )
    kind match {
      case "RiemannFirstU" => c( "Riemann curvature tensor", "R", List( Lower, Lower, Lower, Lower ), cv.flat4( cv.riemannLower ), RiemannLike )
      case "RiemannSecondU" => c( "Riemann curvature tensor", "R", List( Upper, Lower, Lower, Lower ), cv.flat4( cv.riemann ), AntiLastPair )
      case "RicciTensorU" => c( "Ricci tensor", "R", ll, cv.flat2( cv.ricci ), Pair )
      case "EinsteinTensorU" => c( "Einstein tensor", "G", ll, cv.flat2( cv.einstein ), Pair )
      case "WeylTensorU" => c( "Weyl tensor", "C", List( Lower, Lower, Lower, Lower ), cv.flat4( cv.weyl ), RiemannLike )
      case "RicciScalarU" => cv.ricciScalar
      case "EinsteinScalarU" => Tidy( Product( Number( 1.0 - m.dimension / 2.0 ), cv.ricciScalar ).visit() )
      case other => ErrorExpr( "unknown tensor " + other )
    }
  }
}
