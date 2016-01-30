import org.scalatest._

/*import util.Random
import util.Random.nextDouble

import galileo.complex._
import galileo.environment._
import galileo.expr._
import galileo.exprhandler._
*/
import galileo.expr._
import galileo.manipulate.Simplify
import galileo.trigonometry.SinF1

class DerivativeTest extends FunSuite {

	// D( sin( x ) / x^2 ) = ( x cos(x) - 2 sin( x ) ) / x^3
	val x = Variable( "x" )
	val g = SinF1( x )
	val h = Square( x )
	val f = Fraction( g, h )
	test( "fraction") {
		val fd = Derivative( f, x )
		assert( fd.visit().simplify.toString == "(x*cos(x)-2.0*sin(x))/x^3.0" )
	}
}
