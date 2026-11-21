import galileo.constants._
import galileo.environment._
import galileo.expr._
import galileo.exprhandler._
import galileo.parser._
import galileo.plot._

import org.scalatest._

class PlotTest extends FunSuite {
  val parser = new Parser()
  val handler = new ExprHandler
  def newEnv() = {
    val genv = new Environment( None )
    genv.set( "pi", new ConstantPi() )
    genv.set( "e", new ConstantE )
    genv.set( "j", new ConstantJ )
    genv.set( "i", new ConstantJ )
    new Environment( Some( genv ) )
  }

  def run( env: Environment, src: String ): Expr = parser.parse( src ) match {
    case parser.Success( exprs, _ ) => exprs.map( e => handler.evaluate( env, e ) ).last
    case err: parser.NoSuccess => fail( err.toString )
  }

  test( "plot of one function produces a figure with sampled points" ) {
    run( newEnv(), "plot(sin(x), x, 0, 2*pi)" ) match {
      case f: Figure => {
        assert( f.series.size == 1 )
        val pts = f.series.head.points
        assert( pts.size == Plot.samples + 1 )
        assert( math.abs( pts.head._1 ) < 1e-12 )
        assert( math.abs( pts.last._1 - 2 * math.Pi ) < 1e-9 )
        val ( x, y ) = pts( Plot.samples / 4 )
        assert( math.abs( y - math.sin( x ) ) < 1e-9 )
      }
      case other => fail( "expected a Figure, got " + other )
    }
  }

  test( "plot of several functions, using user-defined variables and a default range" ) {
    val env = newEnv()
    run( env, "a = 3" )
    run( env, "f = a*x^2 - 1" )
    run( env, "plot(f, cos(x), x)" ) match {
      case f: Figure => {
        assert( f.series.size == 2 )
        val ( x, y ) = f.series.head.points( 100 )
        assert( math.abs( y - ( 3 * x * x - 1 ) ) < 1e-9 )
        assert( f.series.head.points.head._1 == -10.0 )
      }
      case other => fail( "expected a Figure, got " + other )
    }
  }

  test( "plot of a derivative" ) {
    run( newEnv(), "plot(deriv(x^3, x), x, -2, 2)" ) match {
      case f: Figure => {
        val ( x, y ) = f.series.head.points( 300 )
        assert( math.abs( y - 3 * x * x ) < 1e-9 )
      }
      case other => fail( "expected a Figure, got " + other )
    }
  }

  test( "poles and invalid points do not break rendering" ) {
    run( newEnv(), "plot(1/x, log(x), x, -1, 1)" ) match {
      case f: Figure => {
        val svg = f.svg()
        assert( svg.startsWith( "<svg" ) && svg.endsWith( "</svg>" ) )
        assert( !svg.contains( "NaN" ) && !svg.contains( "Infinity" ) )
        assert( f.mimeBundle.keySet == Set( "text/plain", "image/svg+xml" ) )
        assert( f.ascii().split( "\n" ).length > 10 )
      }
      case other => fail( "expected a Figure, got " + other )
    }
  }

  test( "parametric plot" ) {
    run( newEnv(), "parametric(cos(t), sin(t), t, 0, 2*pi)" ) match {
      case f: Figure => f.series.head.finite.foreach { case ( x, y ) => assert( math.abs( x * x + y * y - 1 ) < 1e-9 ) }
      case other => fail( "expected a Figure, got " + other )
    }
  }

  test( "bad plot arguments give an error, not an exception" ) {
    assert( run( newEnv(), "plot(sin(x), x, 1, 1)" ).isInstanceOf[ ErrorExpr ] )
    assert( run( newEnv(), "plot(sin(x))" ).isInstanceOf[ ErrorExpr ] )
  }

  test( "existing evaluation is unchanged" ) {
    assert( handler.eval( newEnv(), parser.parse( "2+3" ).get.head ) == "5.0" )
  }
}
