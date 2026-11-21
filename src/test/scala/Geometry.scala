import galileo.constants._
import galileo.environment._
import galileo.expr._
import galileo.exprhandler._
import galileo.geometry._
import galileo.parser._
import galileo.plot._
import galileo.tensor._

import org.scalatest._

class GeometryTest extends FunSuite {
  val parser = new Parser()
  val handler = new ExprHandler
  def newEnv() = {
    val genv = new Environment( None )
    genv.set( "pi", new ConstantPi() )
    genv.set( "e", new ConstantE )
    new Environment( Some( genv ) )
  }

  def run( env: Environment, src: String ): Expr = parser.parse( src ) match {
    case parser.Success( exprs, _ ) => exprs.map( e => handler.evaluate( env, e ) ).last
    case err: parser.NoSuccess => fail( err.toString )
  }
  def run( src: String ): Expr = run( newEnv(), src )

  // numerical value of an expression with the given variable values
  def num( e: Expr, vals: ( String, Double )* ): Double = {
    val env = new Environment()
    vals.foreach { case ( n, v ) => env.set( n, Number( v ) ) }
    Compiled.number( e, env )
  }

  test( "user defined metric, Christoffel symbols of polar coordinates" ) {
    val env = newEnv()
    assert( run( env, "g = metric([r phi], [1 0; 0 r^2])" ).toString == "ds^2 = dr^2 + r^2.0*dphi^2" )
    run( env, "christoffel(g)" ) match {
      case c: Components =>
        assert( c.nonZero.size == 2 )
        assert( c.valueAt( 0, 1, 1 ).toString == "(-1.0)*r" )
        assert( c.valueAt( 1, 0, 1 ).toString == "1.0/r" )
        assert( c.valueAt( 1, 1, 0 ).toString == "1.0/r" ) // symmetric
      case other => fail( "expected components, got " + other )
    }
    assert( run( env, "gausscurvature(g)" ) == Number( 0 ) )
  }

  test( "coordinates stay symbolic even when a variable with that name has a value" ) {
    val env = newEnv()
    run( env, "r = 3" )
    assert( run( env, "metric([r phi], [1 0; 0 r^2])" ).toString == "ds^2 = dr^2 + r^2.0*dphi^2" )
  }

  test( "curvature of the sphere, torus, hyperbolic plane and saddle" ) {
    assert( run( "gausscurvature(metric.generate(two-sphere))" ).toString == "1.0/r^2.0" )
    assert( run( "ricciscalar(metric.generate(two-sphere))" ).toString == "2.0/r^2.0" )
    assert( run( "gausscurvature(metric.generate(hyperbolic))" ) == Number( -1 ) )
    val torus = run( "gausscurvature(inducedmetric([(R+a*cos(u))*cos(v) (R+a*cos(u))*sin(v) a*sin(u)], [u v]))" )
    assert( torus.toString == "cos(u)/(a*(R+a*cos(u)))" )
    val saddle = run( "gausscurvature(inducedmetric([u v u^2-v^2], [u v]))" )
    for ( ( u, v ) <- List( ( 0.0, 0.0 ), ( 0.3, -1.2 ), ( 2.0, 0.5 ) ) )
      assert( math.abs( num( saddle, "u" -> u, "v" -> v ) + 4 / math.pow( 1 + 4 * u * u + 4 * v * v, 2 ) ) < 1e-12 )
  }

  test( "Schwarzschild: vacuum solution, Kretschmann scalar, Flamm's paraboloid" ) {
    run( "riccitensor(metric.generate(schwarzschild))" ) match {
      case c: Components => assert( c.nonZero.isEmpty )
      case other => fail( "expected components, got " + other )
    }
    assert( run( "kretschmann(metric.generate(schwarzschild))" ).toString == "12.0*r_s^2.0/r^6.0" )
    // the spatial slice of Schwarzschild is the surface z = 2 sqrt(r_s (r - r_s))
    val flamm = run( "inducedmetric([r*cos(phi) r*sin(phi) 2*sqrt(r_s*(r-r_s))], [r phi])" ).asInstanceOf[ Metric ]
    assert( math.abs( num( flamm.valueAt( 0, 0 ), "r" -> 3, "r_s" -> 1 ) - 1.5 ) < 1e-12 )
    assert( math.abs( num( flamm.valueAt( 1, 1 ), "r" -> 3, "r_s" -> 1 ) - 9 ) < 1e-12 )
  }

  test( "new curvature code agrees with the original tensor package" ) {
    val g = Metric.threeSphere( Variable( "r" ) )
    val cv = Curvature( g )
    val old = RicciTensor( g )
    for ( a <- 0 until 3; b <- 0 until 3 ) {
      val vals = Seq( "r" -> 2.0, "psi" -> 0.7, "theta" -> 1.1, "phi" -> 0.3 )
      assert( math.abs( num( cv.ricci( a )( b ), vals: _* ) - num( old.valueAt( a, b ), vals: _* ) ) < 1e-9 )
    }
    assert( math.abs( num( cv.ricciScalar, "r" -> 2.0, "psi" -> 0.7, "theta" -> 1.1 ) - 6.0 / 4 ) < 1e-9 )
  }

  test( "geodesic equations of the sphere" ) {
    run( "geodesics(metric.generate(two-sphere))" ) match {
      case Equations( _, eqs ) =>
        assert( eqs.map( _._1.toString ) == List( "theta''", "phi''" ) )
        assert( eqs.head._2.toString == "phi'^2.0*cos(theta)*sin(theta)" )
      case other => fail( "expected equations, got " + other )
    }
  }

  test( "numerical geodesics on the sphere are great circles" ) {
    val env = newEnv()
    run( env, "r = 1" )
    // start on the equator heading north-east: the path must stay on a great circle,
    // i.e. in a plane through the centre
    run( env, "geodesic(metric.generate(two-sphere), [pi/2 0], [1 1], 6, [sin(theta)*cos(phi) sin(theta)*sin(phi) cos(theta)])" ) match {
      case f: Figure3D =>
        val pts = f.curves.head.points
        assert( pts.size > 100 )
        val n = ( 0.0, 1 / math.sqrt( 2 ), 1 / math.sqrt( 2 ) ) // normal of the plane spanned by (1,0,0) and the start velocity (0,1,-1)
        for ( ( x, y, z ) <- pts ) {
          assert( math.abs( x * x + y * y + z * z - 1 ) < 1e-6 )
          assert( math.abs( x * n._1 + y * n._2 + z * n._3 ) < 1e-5 )
        }
      case other => fail( "expected a 3D figure, got " + other )
    }
  }

  test( "geodesic asks for missing parameter values" ) {
    val r = run( "geodesic(metric.generate(schwarzschild), [0 10 pi/2 0], [1 0 0 0.01], 10, [r*cos(phi) r*sin(phi)])" )
    assert( r.toString.contains( "r_s" ) )
  }

  test( "ODE solver: harmonic oscillator and Lorenz attractor" ) {
    val env = newEnv()
    run( env, "odesolve([v, -x], [x v], t, [1 0], 0, 2*pi)" ) match {
      case m: galileo.linalg.Matrix =>
        assert( math.abs( num( m( 0, 0 ) ) - 1 ) < 1e-9 )
        assert( math.abs( num( m( 0, 1 ) ) ) < 1e-9 )
      case other => fail( "expected a row vector, got " + other )
    }
    run( env, "sigma = 10; rho = 28; beta = 8/3" )
    run( env, "odeplot([sigma*(y-x) x*(rho-z)-y x*y-beta*z], [x y z], t, [1 1 1], 0, 30, [x y z])" ) match {
      case f: Figure3D => assert( f.curves.head.points.size > 1000 )
      case other => fail( "expected a 3D figure, got " + other )
    }
    assert( run( env, "odeplot([v, -k*x], [x v], t, [1 0], 0, 10, [x v])" ).toString.contains( "k" ) )
  }

  test( "GR perihelion precession: the orbit equation precesses by about 3 pi r_s / p per orbit" ) {
    // u'' + u = r_s/(2 h^2) + 3/2 r_s u^2, starting at perihelion; after one radial period
    // the next perihelion is reached at 2 pi + delta
    val env = newEnv()
    run( env, "r_s = 1; h = sqrt(15); rp = 20" )
    val f = run( env, "odeplot([w r_s/(2*h^2)+1.5*r_s*u^2-u], [u w], phi, [1/rp 0], 0, 2.6*pi, [phi u], 26000)" ).asInstanceOf[ Figure ]
    val pts = f.series.head.points
    // perihelion = maximum of u after the aphelion
    val later = pts.filter( _._1 > 1.5 * math.Pi )
    val peri = later.maxBy( _._2 )._1
    val delta = peri - 2 * math.Pi
    val p = 30.0 // semi-latus rectum for h^2 = 15 with GM = 1/2
    assert( math.abs( delta - 3 * math.Pi / p ) / ( 3 * math.Pi / p ) < 0.2 )
  }

  test( "matrix elements can be separated by commas" ) {
    assert( run( "[1, -2, 3]" ).toString == "1.0\t-2.0\t3.0" )
    assert( run( "[1 2; 3 4]" ).toString == "1.0\t2.0\n3.0\t4.0" )
  }

  test( "LaTeX rendering" ) {
    assert( Latex( run( "x = 2*r^2*sin(theta)^2" ) ) == "2 r^{2} \\sin^{2}\\left(\\theta\\right)" )
    assert( Latex( Fraction( Variable( "r_s" ), Variable( "r" ) ) ) == "\\frac{r_{s}}{r}" )
    run( "christoffel(metric.generate(two-sphere))" ) match {
      case c: Components => assert( c.mimeBundle( "text/latex" ).contains( "\\Gamma{}^{\\theta}{}_{\\phi}{}_{\\phi} &= -\\cos\\left(\\theta\\right) \\sin\\left(\\theta\\right)" ) )
      case other => fail( "expected components, got " + other )
    }
  }
}
