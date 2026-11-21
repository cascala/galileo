package galileo.geometry

import galileo.environment.Environment
import galileo.expr._
import galileo.linalg.DenseMatrix
import galileo.plot.{Curve3D, Figure, Figure3D, Series}
import galileo.tensor._

// Numerical integration of systems of ordinary differential equations dy/dt = f(t, y)
// with the classical fourth order Runge-Kutta method
object Ode {
  val defaultSteps = 20000
  val maxPlotPoints = 1500

  type Rhs = ( Double, Array[ Double ] ) => Array[ Double ]

  // returns the samples (t, y) including the start; stops early if the solution blows up
  def rk4( f: Rhs, y0: Array[ Double ], t0: Double, t1: Double, steps: Int ): Vector[ ( Double, Array[ Double ] ) ] = {
    val n = y0.length
    val h = ( t1 - t0 ) / steps
    val out = Vector.newBuilder[ ( Double, Array[ Double ] ) ]
    var y = y0.clone()
    var t = t0
    out += ( ( t, y.clone() ) )
    def axpy( a: Array[ Double ], s: Double, b: Array[ Double ] ) = { val r = new Array[ Double ]( n ); var i = 0; while ( i < n ) { r( i ) = a( i ) + s * b( i ); i += 1 }; r }
    var i = 0
    var ok = true
    while ( i < steps && ok ) {
      val k1 = f( t, y )
      val k2 = f( t + h / 2, axpy( y, h / 2, k1 ) )
      val k3 = f( t + h / 2, axpy( y, h / 2, k2 ) )
      val k4 = f( t + h, axpy( y, h, k3 ) )
      val next = new Array[ Double ]( n )
      var j = 0
      while ( j < n ) { next( j ) = y( j ) + h / 6 * ( k1( j ) + 2 * k2( j ) + 2 * k3( j ) + k4( j ) ); j += 1 }
      ok = next.forall( v => !v.isNaN && !v.isInfinite && math.abs( v ) < 1e12 )
      // stop at singularities (e.g. a geodesic reaching r = 0 or a coordinate horizon), where
      // a single step suddenly jumps by a large amount
      if ( ok ) {
        var jump = 0.0; var size = 1.0; var j = 0
        while ( j < n ) { jump = math.max( jump, math.abs( next( j ) - y( j ) ) ); size = math.max( size, math.abs( y( j ) ) ); j += 1 }
        ok = jump < 0.25 * size
      }
      if ( ok ) { y = next; t += h; out += ( ( t, y.clone() ) ) }
      i += 1
    }
    out.result()
  }

  def thin[ A ]( v: Vector[ A ], max: Int = maxPlotPoints ): Vector[ A ] =
    if ( v.size <= max ) v else { val k = math.ceil( v.size.toDouble / max ).toInt; ( v.indices by k ).map( v ).toVector :+ v.last }

  // Turn trajectories into a 2D or 3D figure, mapping each state through the expressions in `map`
  def figure( trajectories: List[ ( String, Vector[ ( Double, Array[ Double ] ) ] ) ], names: Seq[ String ], map: List[ Expr ], env: Environment, labels: List[ String ], equalAspect: Boolean = false ): Expr = {
    val fs = map.map( m => Compiled( m, names, env ) )
    def at( t: Double, y: Array[ Double ] ) = { val a = new Array[ Double ]( y.length + 1 ); Array.copy( y, 0, a, 0, y.length ); a( y.length ) = t; a }
    map.size match {
      case 2 => Figure( trajectories.map { case ( label, pts ) =>
        Series( label, thin( pts ).map { case ( t, y ) => val a = at( t, y ); ( fs( 0 )( a ), fs( 1 )( a ) ) } )
      }, labels( 0 ), "", labels( 1 ), equalAspect )
      case 3 => Figure3D( Nil, trajectories.map { case ( label, pts ) =>
        Curve3D( label, thin( pts ).map { case ( t, y ) => val a = at( t, y ); ( fs( 0 )( a ), fs( 1 )( a ), fs( 2 )( a ) ) } )
      }, "", ( labels( 0 ), labels( 1 ), labels( 2 ) ), equalAspect )
      case _ => ErrorExpr( "the plot map must have 2 entries (2D) or 3 entries (3D), e.g. [x y] or [x y z]" )
    }
  }

  def numbers( e: Expr, env: Environment ): Option[ List[ Double ] ] = Args.list( e ).map( _.map( x => Compiled.number( x, env ) ) ).filter( _.forall( !_.isNaN ) )
  def number( e: Expr, env: Environment ): Option[ Double ] = Some( Compiled.number( e, env ) ).filter( !_.isNaN )
}

// odeplot( [f1 f2 ...], [y1 y2 ...], t, [y1(t0) y2(t0) ...], t0, t1, [X Y] or [X Y Z] (, steps) )
// odesolve( [f1 f2 ...], [y1 y2 ...], t, [y1(t0) y2(t0) ...], t0, t1 (, steps) )  -> [y1(t1) y2(t1) ...]
case class OdeU( plot: Boolean, args: List[ Expr ] ) extends Expr with Statement {
  def info( env: Option[ Environment ] = None ) = ( if ( plot ) "odeplot" else "odesolve" ) + "(" + args.mkString( "," ) + ")"
  private def usage = if ( plot ) "usage: odeplot([f1 f2], [y1 y2], t, [y1_0 y2_0], t0, t1, [X Y]) where X, Y (and Z for 3D) are expressions in t, y1, y2"
    else "usage: odesolve([f1 f2], [y1 y2], t, [y1_0 y2_0], t0, t1)"

  override def visit( env: Option[ Environment ] = None ): Expr = {
    val e = env.getOrElse( new Environment() )
    val ( core, map, steps ) = ( plot, args ) match {
      case ( true, List( a, b, c, d, t0, t1, m ) ) => ( List( a, b, c, d, t0, t1 ), Some( m ), None )
      case ( true, List( a, b, c, d, t0, t1, m, s ) ) => ( List( a, b, c, d, t0, t1 ), Some( m ), Some( s ) )
      case ( false, List( a, b, c, d, t0, t1 ) ) => ( List( a, b, c, d, t0, t1 ), None, None )
      case ( false, List( a, b, c, d, t0, t1, s ) ) => ( List( a, b, c, d, t0, t1 ), None, Some( s ) )
      case _ => return ErrorExpr( usage )
    }
    val List( rhsE, varsE, tE, y0E, t0E, t1E ) = core
    val result = for {
      rhs <- Args.list( Args.symbolic( rhsE, env ) )
      vars <- Args.variables( varsE )
      t <- Some( tE ).collect { case v: Variable => v }
      y0 <- Ode.numbers( y0E.visit( env ), e )
      t0 <- Ode.number( t0E.visit( env ), e )
      t1 <- Ode.number( t1E.visit( env ), e )
      if rhs.size == vars.size && y0.size == vars.size
    } yield {
      val names = vars.map( _.name ) :+ t.name
      val free = rhs.flatMap( r => Free( r, names.toSet, e ) ).distinct
      if ( free.nonEmpty ) return ErrorExpr( "give values to " + free.mkString( ", " ) + " first, e.g. " + free.head + " = 1" )
      val fs = rhs.map( r => Compiled( r, names, e ) ).toArray
      val n = vars.size
      val f: Ode.Rhs = ( tt, y ) => {
        val a = new Array[ Double ]( n + 1 ); Array.copy( y, 0, a, 0, n ); a( n ) = tt
        fs.map( g => g( a ) )
      }
      val nSteps = steps.flatMap( s => Ode.number( s.visit( env ), e ) ).map( _.toInt ).getOrElse( Ode.defaultSteps )
      val traj = Ode.rk4( f, y0.toArray, t0, t1, math.max( 1, nSteps ) )
      map match {
        case Some( m ) => Args.list( Args.symbolic( m, env ) ) match {
          case Some( ms ) => {
            // polar-style maps such as [cos(phi)/u sin(phi)/u] (orbits) are drawn with equal axes
            val polar = ms.size == 2 && ms( 0 ).toString.contains( "cos(" ) && ms( 1 ).toString.contains( "sin(" )
            Ode.figure( List( ( "solution", traj ) ), names, ms, e, ms.map( _.toString ), equalAspect = polar )
          }
          case None => ErrorExpr( usage )
        }
        case None => DenseMatrix( List( traj.last._2.toList.map( Number( _ ) ) ) )
      }
    }
    result.getOrElse( ErrorExpr( usage ) )
  }
}

// geodesic( g, [x1_0 x2_0 ...], [v1_0 v2_0 ...], s1 (, [X Y (Z)] (, steps)) )
// Integrates the geodesic equations numerically from the initial position and velocity
// for affine parameter 0..s1. Several initial velocities (one per row) give several geodesics.
// The optional map turns coordinates into points to plot, e.g. the embedding of a surface.
case class GeodesicU( args: List[ Expr ] ) extends Expr with Statement {
  def info( env: Option[ Environment ] = None ) = "geodesic(" + args.mkString( "," ) + ")"
  private val usage = "usage: geodesic(g, [x0 y0], [vx vy], s1) or geodesic(g, x0, v0, s1, [X Y Z]) with X, Y, Z expressions in the coordinates"

  override def visit( env: Option[ Environment ] = None ): Expr = {
    val e = env.getOrElse( new Environment() )
    val ( gE, x0E, v0E, s1E, mapE, stepsE ) = args match {
      case List( a, b, c, d ) => ( a, b, c, d, None, None )
      case List( a, b, c, d, m ) => ( a, b, c, d, Some( m ), None )
      case List( a, b, c, d, m, s ) => ( a, b, c, d, Some( m ), Some( s ) )
      case _ => return ErrorExpr( usage )
    }
    val metric = Args.metric( gE, env ) match {
      case Right( m ) => m
      case Left( err ) => return ErrorExpr( "geodesic: " + err )
    }
    val n = metric.dimension
    val xs = metric.variables.map( _.name.trim )
    val x0 = Ode.numbers( x0E.visit( env ), e ).filter( _.size == n ).getOrElse( return ErrorExpr( "geodesic: the start point needs " + n + " numbers" ) )
    val v0s: List[ List[ Double ] ] = v0E.visit( env ) match {
      case m: galileo.linalg.Matrix if m.numCols == n => m.toDenseMatrix.rows.map( r => r.map( x => Compiled.number( x, e ) ) )
      case _ => return ErrorExpr( "geodesic: the start velocity needs " + n + " numbers (or one row of " + n + " per geodesic)" )
    }
    if ( v0s.flatten.exists( _.isNaN ) ) return ErrorExpr( "geodesic: the start velocity must be numbers" )
    val s1 = Ode.number( s1E.visit( env ), e ).getOrElse( return ErrorExpr( "geodesic: the length s1 must be a number" ) )
    val steps = stepsE.flatMap( s => Ode.number( s.visit( env ), e ) ).map( _.toInt ).getOrElse( Ode.defaultSteps / 4 )

    // compile the non-zero Christoffel symbols once
    val cv = Curvature( metric )
    val terms: Array[ ( Int, Int, Int, Compiled.Fn ) ] = ( for ( a <- 0 until n; b <- 0 until n; c <- b until n ) yield {
      val g = cv.gamma( a, b, c )
      g match {
        case Number( 0.0 ) => None
        case _ => Some( ( a, b, c, Compiled( g, xs, e ) ) )
      }
    } ).flatten.toArray
    val free = terms.toList.flatMap { case ( a, b, c, _ ) => Free( cv.gamma( a, b, c ), xs.toSet, e ) }.distinct
    if ( free.nonEmpty ) return ErrorExpr( "geodesic: give values to " + free.mkString( ", " ) + " first, e.g. " + free.head + " = 1" )
    val f: Ode.Rhs = ( _, y ) => {
      val pos = y.take( n )
      val acc = new Array[ Double ]( n )
      var k = 0
      while ( k < terms.length ) {
        val ( a, b, c, g ) = terms( k )
        val w = if ( b == c ) 1.0 else 2.0
        acc( a ) -= w * g( pos ) * y( n + b ) * y( n + c )
        k += 1
      }
      y.slice( n, 2 * n ) ++ acc
    }
    val trajs = v0s.zipWithIndex.map { case ( v0, i ) =>
      ( "geodesic " + ( i + 1 ), Ode.rk4( f, ( x0 ++ v0 ).toArray, 0.0, s1, math.max( 1, steps ) ) )
    }
    val names = xs ++ xs.map( _ + "'" ) :+ "s"
    val map = mapE match {
      case Some( m ) => Args.list( Args.symbolic( m, env ) ).getOrElse( return ErrorExpr( usage ) )
      case None if n == 2 || n == 3 => metric.variables
      case None => return ErrorExpr( "geodesic: for a " + n + "D metric, give a map to 2 or 3 plot coordinates, e.g. [r*cos(phi) r*sin(phi)]" )
    }
    Ode.figure( trajs, names, map, e, map.map( _.toString ), equalAspect = true )
  }
}

// free variables of an expression that have no value in the environment
object Free {
  def apply( e: Expr, bound: Set[ String ], env: Environment ): List[ String ] = {
    val out = scala.collection.mutable.LinkedHashSet[ String ]()
    def walk( x: Expr, depth: Int ): Unit = if ( depth < 64 ) x match {
      case Variable( n ) if bound( n.trim ) || n == "pi" || n == "e" => ()
      case Variable( n ) => env.get( n ) match {
        case Some( Variable( m ) ) if m == n => out += n
        case Some( v ) => walk( v, depth + 1 )
        case None => out += n.trim
      }
      case s: Sum => s.terms.foreach( walk( _, depth + 1 ) )
      case p: Product => p.factors.foreach( walk( _, depth + 1 ) )
      case Fraction( a, b ) => { walk( a, depth + 1 ); walk( b, depth + 1 ) }
      case Power( a, b ) => { walk( a, depth + 1 ); walk( b, depth + 1 ) }
      case f: galileo.expr.FunF1 => walk( f.e, depth + 1 )
      case _ => ()
    }
    walk( e, 0 )
    out.toList
  }
}
