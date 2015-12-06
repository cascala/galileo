import org.scalatest._

import galileo.environment.Environment
import galileo.expr._
import galileo.exprhandler._
import galileo.parser._
import galileo.manipulate.{Expand,Simplify}
import galileo.trigonometry.CosF1

class FractionTest extends FunSuite {
	//val parser = new Parser()
  	//val handler = new ExprHandler
	//import parser.{ Success, NoSuccess }

	val va = Variable( "a" )
	val vb = Variable( "b" )
	test( "extractFactor") {
		val f = Fraction(va,Square(vb))
		val possibleFactor = Fraction( Number( 1 ), vb )
		f.extractFactor( Fraction(Number(1),vb)) match {
			case Some( f ) => assert(f.toString == "a/b")
			case None => {
				info( "f.numerator.extractFactor():" + f.numerator.extractFactor( possibleFactor.numerator ) )
				info( "f.denominator.extractFactor():" + f.denominator.extractFactor( possibleFactor.denominator ) )
				fail( "expected:a/b")
			}
		}
	}
}
