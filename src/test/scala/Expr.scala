import org.scalatest._

import util.Random
import util.Random.nextDouble

import galileo.complex._
import galileo.environment._
import galileo.expr._
import galileo.exprhandler._

class ExprTest001 extends FunSuite {
	val e = Product( Variable( "x" ), Number( 3 ) )

	val expected = Map[Expr,Expr](
		Number( 1 ) -> Number( 1 ),
		e.derive( Variable( "x") ) -> Number( 3 ) 
	)

	val handler = new ExprHandler
	val genv = new Environment( None ) // Global env

	test( "expected") {
		expected foreach { case( a, b ) => 
			val r1 = handler.eval( genv, a )
			val r2 = handler.eval( genv, b )

			assert( a == b || r1 == r2 ) //rassert( math.abs( a.evaluate - b.evaluate ) < 1E-10 ) )
		}
	}
	//assert( Number( 0 ) == Complex( Number( 10 ), Number( 5 ) ) * Number( 0 ) )
} // class ExprTest001
