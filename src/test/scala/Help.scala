import galileo.constants._
import galileo.environment._
import galileo.expr._
import galileo.exprhandler._
import galileo.help._
import galileo.parser._

import org.scalatest._

class HelpTest extends FunSuite {
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
    case err: parser.NoSuccess => fail( src + ": " + err.toString )
  }

  test( "help lists the commands by topic" ) {
    run( newEnv(), "help" ) match {
      case h: HelpText => {
        assert( h.text.contains( "Plotting: " ) && h.text.contains( "plot3d" ) && h.text.contains( "gausscurvature" ) )
        assert( h.mimeBundle.contains( "text/markdown" ) )
      }
      case other => fail( "expected help, got " + other )
    }
  }

  test( "help(name) describes one command, including aliases" ) {
    assert( run( newEnv(), "help(plot3d)" ).toString.startsWith( "plot3d(f, x, a, b, y, c, d)" ) )
    assert( run( newEnv(), "help(ricciscalar)" ).toString.contains( "Ricci" ) )
    assert( run( newEnv(), "help(metric.generate)" ).toString.contains( "two-sphere" ) )
    assert( run( newEnv(), "help(nosuchthing)" ).isInstanceOf[ ErrorExpr ] )
  }

  test( "variables starting with help still work" ) {
    assert( run( newEnv(), "helper = 3" ).toString == "3.0" )
  }

  test( "every help example runs without an error" ) {
    val failures = for ( e <- Help.entries if e.example.nonEmpty && e.name != "load" ;
      msg <- ( try parser.parse( e.example ) match {
        case parser.Success( exprs, _ ) => { val env = newEnv(); exprs.map( x => handler.evaluate( env, x ) ).last match {
          case ErrorExpr( m ) => Some( m )
          case _ => None
        } }
        case err: parser.NoSuccess => Some( err.msg )
      } catch { case ex: Throwable => Some( ex.toString ) } ) ) yield e.example + " -> " + msg
    assert( failures.isEmpty, failures.mkString( "\n" ) )
  }
}
