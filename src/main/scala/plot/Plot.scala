package galileo.plot

import galileo.constants._
import galileo.environment.Environment
import galileo.expr._
import galileo.trigonometry._

// Anything that can render itself as rich output (e.g. in a Jupyter notebook).
// The map goes from MIME type to content, e.g. "image/svg+xml" -> "<svg ...>".
// Values of JSON MIME types (such as application/vnd.plotly.v1+json) are JSON text.
trait Displayable {
  def mimeBundle: Map[ String, String ]
}

// Shared helpers for the plotting commands
object Plot {
  val samples = 400 // points per 2D curve
  val curve3DSamples = 600 // points per 3D curve
  val gridSize = 41 // grid lines per direction for surfaces

  // Evaluate the two ends of a range, or explain why that is impossible
  def range( cmd: String, from: Expr, to: Expr, env: Environment ): Either[ ErrorExpr, ( Double, Double ) ] = {
    val ( a, b ) = ( Numeric.eval( from, env ), Numeric.eval( to, env ) )
    if ( a.isNaN || b.isNaN || a.isInfinite || b.isInfinite || a == b )
      Left( ErrorExpr( cmd + ": the range " + from + " .. " + to + " must be two different numbers" ) )
    else Right( ( a, b ) )
  }

  def steps( a: Double, b: Double, n: Int ): Vector[ Double ] = ( 0 to n ).map( i => a + ( b - a ) * i / n ).toVector

  // plot( f, x ) uses a default range, plot( f, g, x, a, b ) an explicit one
  def fromArgs( args: List[ Expr ] ): Expr = args.reverse match {
    case to :: from :: ( v: Variable ) :: rest if rest.nonEmpty => Plot( rest.reverse, v, from, to )
    case ( v: Variable ) :: rest if rest.nonEmpty => Plot( rest.reverse, v, Number( -10 ), Number( 10 ) )
    case _ => ErrorExpr( "usage: plot(f, x) or plot(f1, f2, ..., x, from, to)" )
  }

  private def usage( msg: String ) = ErrorExpr( "usage: " + msg )

  def parametricFromArgs( args: List[ Expr ] ): Expr = args match {
    case List( fx, fy, v: Variable, from, to ) => ParametricPlot( fx, fy, v, from, to )
    case _ => usage( "parametric(fx, fy, t, from, to)" )
  }

  def plot3dFromArgs( args: List[ Expr ] ): Expr = args match {
    case List( f, x: Variable, a, b, y: Variable, c, d ) => Plot3D( f, x, a, b, y, c, d )
    case _ => usage( "plot3d(f, x, from, to, y, from, to)" )
  }

  def surfaceFromArgs( args: List[ Expr ] ): Expr = args match {
    case List( fx, fy, fz, u: Variable, a, b, v: Variable, c, d ) => ParametricSurface( fx, fy, fz, u, a, b, v, c, d )
    case _ => usage( "surface(fx, fy, fz, u, from, to, v, from, to)" )
  }

  def parametric3dFromArgs( args: List[ Expr ] ): Expr = args match {
    case List( fx, fy, fz, t: Variable, a, b ) => Parametric3D( fx, fy, fz, t, a, b )
    case _ => usage( "parametric3d(fx, fy, fz, t, from, to)" )
  }
}

// Common parts of the plot commands: they are statements that evaluate to a figure
trait PlotCommand extends Expr with Statement {
  def name: String
  def args: List[ Expr ]
  def info( env: Option[ Environment ] = None ) = name + "(" + args.mkString( "," ) + ")"
  override def toString() = name + "(" + args.mkString( "," ) + ")"
  def render( env: Environment ): Expr
  override def visit( env: Option[ Environment ] = None ): Expr =
    try render( env.getOrElse( new Environment() ) )
    catch { case e: Exception => ErrorExpr( name + ": " + e.getMessage ) }
}

// plot( f1, f2, ..., x, from, to )
case class Plot( exprs: List[ Expr ], variable: Variable, from: Expr, to: Expr ) extends PlotCommand {
  def name = "plot"
  def args = exprs ++ List( variable, from, to )
  def render( e: Environment ): Expr = Plot.range( name, from, to, e ) match {
    case Left( err ) => err
    case Right( ( a, b ) ) =>
      val xs = Plot.steps( math.min( a, b ), math.max( a, b ), Plot.samples )
      val series = exprs.map { expr =>
        val f = Numeric.compile( expr, variable.name, e )
        Series( expr.toString, xs.map( x => ( x, f( x ) ) ) )
      }
      Figure( series, variable.name, "" )
  }
}

// parametric( fx, fy, t, from, to )
case class ParametricPlot( fx: Expr, fy: Expr, variable: Variable, from: Expr, to: Expr ) extends PlotCommand {
  def name = "parametric"
  def args = List( fx, fy, variable, from, to )
  def render( e: Environment ): Expr = Plot.range( name, from, to, e ) match {
    case Left( err ) => err
    case Right( ( a, b ) ) =>
      val gx = Numeric.compile( fx, variable.name, e )
      val gy = Numeric.compile( fy, variable.name, e )
      val pts = Plot.steps( a, b, Plot.samples ).map( t => ( gx( t ), gy( t ) ) )
      Figure( List( Series( "(" + fx + ", " + fy + ")", pts ) ), "", "", equalAspect = true )
  }
}

// plot3d( f, x, a, b, y, c, d ): the surface z = f(x, y)
case class Plot3D( f: Expr, x: Variable, a: Expr, b: Expr, y: Variable, c: Expr, d: Expr ) extends PlotCommand {
  def name = "plot3d"
  def args = List( f, x, a, b, y, c, d )
  def render( e: Environment ): Expr = ( Plot.range( name, a, b, e ), Plot.range( name, c, d, e ) ) match {
    case ( Left( err ), _ ) => err
    case ( _, Left( err ) ) => err
    case ( Right( ( x0, x1 ) ), Right( ( y0, y1 ) ) ) =>
      val g = Numeric.compile2( f, x.name, y.name, e )
      val n = Plot.gridSize - 1
      val xs = Plot.steps( x0, x1, n )
      val ys = Plot.steps( y0, y1, n )
      val gx = xs.map( xv => ys.map( _ => xv ) )
      val gy = xs.map( _ => ys )
      val gz = xs.map( xv => ys.map( yv => g( xv, yv ) ) )
      Figure3D( List( Surface3D( f.toString, gx, gy, gz ) ), Nil, "", ( x.name, y.name, "z" ) )
  }
}

// surface( fx, fy, fz, u, a, b, v, c, d ): a parametric surface
case class ParametricSurface( fx: Expr, fy: Expr, fz: Expr, u: Variable, a: Expr, b: Expr, v: Variable, c: Expr, d: Expr ) extends PlotCommand {
  def name = "surface"
  def args = List( fx, fy, fz, u, a, b, v, c, d )
  def render( e: Environment ): Expr = ( Plot.range( name, a, b, e ), Plot.range( name, c, d, e ) ) match {
    case ( Left( err ), _ ) => err
    case ( _, Left( err ) ) => err
    case ( Right( ( u0, u1 ) ), Right( ( v0, v1 ) ) ) =>
      val n = Plot.gridSize - 1
      val us = Plot.steps( u0, u1, n )
      val vs = Plot.steps( v0, v1, n )
      def grid( f: Expr ) = { val g = Numeric.compile2( f, u.name, v.name, e ); us.map( uu => vs.map( vv => g( uu, vv ) ) ) }
      Figure3D( List( Surface3D( "(" + fx + ", " + fy + ", " + fz + ")", grid( fx ), grid( fy ), grid( fz ) ) ), Nil, "", equalAspect = true )
  }
}

// parametric3d( fx, fy, fz, t, a, b ): a curve in space
case class Parametric3D( fx: Expr, fy: Expr, fz: Expr, t: Variable, a: Expr, b: Expr ) extends PlotCommand {
  def name = "parametric3d"
  def args = List( fx, fy, fz, t, a, b )
  def render( e: Environment ): Expr = Plot.range( name, a, b, e ) match {
    case Left( err ) => err
    case Right( ( t0, t1 ) ) =>
      val ( gx, gy, gz ) = ( Numeric.compile( fx, t.name, e ), Numeric.compile( fy, t.name, e ), Numeric.compile( fz, t.name, e ) )
      val pts = Plot.steps( t0, t1, Plot.curve3DSamples ).map( tt => ( gx( tt ), gy( tt ), gz( tt ) ) )
      Figure3D( Nil, List( Curve3D( "(" + fx + ", " + fy + ", " + fz + ")", pts ) ), "", equalAspect = true )
  }
}

// show( p1, p2, ... ): overlay figures of the same kind
case class Show( exprs: List[ Expr ] ) extends PlotCommand {
  def name = "show"
  def args = exprs
  def render( e: Environment ): Expr = {
    val figures = exprs.map( x => x.visit( Some( e ) ) )
    figures.collectFirst { case err: ErrorExpr => err }.getOrElse {
      figures match {
        case Nil => ErrorExpr( "usage: show(figure1, figure2, ...)" )
        case fs if fs.forall( _.isInstanceOf[ Figure ] ) => fs.map( _.asInstanceOf[ Figure ] ).reduce( _ overlay _ )
        case fs if fs.forall( _.isInstanceOf[ Figure3D ] ) => fs.map( _.asInstanceOf[ Figure3D ] ).reduce( _ overlay _ )
        case _ => ErrorExpr( "show: all arguments must be 2D plots, or all 3D plots" )
      }
    }
  }
}

// Numerical evaluation of symbolic expressions, used for plotting
object Numeric {
  private val maxDepth = 64

  // Evaluate an expression without free variables to a double (NaN if that is impossible)
  def eval( e: Expr, env: Environment ): Double = compileN( e, Nil, env )( Array() )

  // Turn expr into a function of the variable called name
  def compile( expr: Expr, name: String, env: Environment ): Double => Double = {
    val f = compileN( expr, List( name ), env )
    ( x: Double ) => f( Array( x ) )
  }

  // Turn expr into a function of two variables
  def compile2( expr: Expr, u: String, v: String, env: Environment ): ( Double, Double ) => Double = {
    val f = compileN( expr, List( u, v ), env )
    ( a: Double, b: Double ) => f( Array( a, b ) )
  }

  // Turn expr into a function of the named variables, given in that order
  def compileN( expr: Expr, names: Seq[ String ], env: Environment ): Array[ Double ] => Double = {
    val free = names.toSet
    val prepared = prepare( expr, free, env, 0 )
    ( values: Array[ Double ] ) => {
      val bound = names.zip( values ).toMap
      try num( prepared, bound, env, 0 ) catch { case _: Throwable => Double.NaN }
    }
  }

  // Substitute user-defined variables (except the free ones) and
  // work out symbolic derivatives once, before sampling
  private def prepare( e: Expr, free: Set[ String ], env: Environment, depth: Int ): Expr = {
    def p( s: Expr ) = prepare( s, free, env, depth + 1 )
    if ( depth > maxDepth ) return e
    e match {
      case Variable( n ) if free.contains( n ) => e
      case Variable( n ) => env.get( n ) match {
        case Some( Variable( m ) ) if m == n => e
        case Some( value ) => p( value )
        case None => e
      }
      case Sum( terms @ _* ) => Sum( terms.map( p ): _* )
      case Product( factors @ _* ) => Product( factors.map( p ): _* )
      case Power( a, b ) => Power( p( a ), p( b ) )
      case Fraction( a, b ) => Fraction( p( a ), p( b ) )
      case SinF1( a ) => SinF1( p( a ) )
      case CosF1( a ) => CosF1( p( a ) )
      case TanF1( a ) => TanF1( p( a ) )
      case AsinF1( a ) => AsinF1( p( a ) )
      case AcosF1( a ) => AcosF1( p( a ) )
      case AtanF1( a ) => AtanF1( p( a ) )
      case LogF1( a ) => LogF1( p( a ) )
      case ExpF1( a ) => ExpF1( p( a ) )
      case Abs( a ) => Abs( p( a ) )
      case Max( as @ _* ) => Max( as.map( p ): _* )
      case Derivative( y, v ) => p( Derivative( p( y ), v ).visit( Some( new Environment() ) ) )
      case other => other
    }
  }

  private def num( e: Expr, bound: Map[ String, Double ], env: Environment, depth: Int ): Double = {
    def n( s: Expr ) = num( s, bound, env, depth )
    e match {
      case Number( v ) => v
      case NumberI( v ) => v.toDouble
      case Variable( v ) if bound.contains( v ) => bound( v )
      case Variable( "pi" ) => math.Pi
      case Variable( "e" ) => math.E
      case c: Constant => c.value match {
        case Number( v ) => v
        case _ => Double.NaN
      }
      case Sum( terms @ _* ) => terms.foldLeft( 0.0 )( ( acc, t ) => acc + n( t ) )
      case Product( factors @ _* ) => factors.foldLeft( 1.0 )( ( acc, f ) => acc * n( f ) )
      case Power( a, b ) => math.pow( n( a ), n( b ) )
      case Fraction( a, b ) => n( a ) / n( b )
      case SinF1( a ) => math.sin( n( a ) )
      case CosF1( a ) => math.cos( n( a ) )
      case TanF1( a ) => math.tan( n( a ) )
      case AsinF1( a ) => math.asin( n( a ) )
      case AcosF1( a ) => math.acos( n( a ) )
      case AtanF1( a ) => math.atan( n( a ) )
      case LogF1( a ) => math.log( n( a ) )
      case ExpF1( a ) => math.exp( n( a ) )
      case Abs( a ) => math.abs( n( a ) )
      case Max( as @ _* ) => as.map( n ).max
      case other if depth < 8 => {
        // Fall back on the symbolic engine, with the free variables bound to their values
        val local = new Environment( Some( env ) )
        bound.foreach { case ( k, v ) => local.set( k, Number( v ) ) }
        other.visit( Some( local ) ).eval() match {
          case Number( v ) => v
          case r if r != other => num( r, bound, env, depth + 1 )
          case _ => Double.NaN
        }
      }
      case _ => Double.NaN
    }
  }
}
