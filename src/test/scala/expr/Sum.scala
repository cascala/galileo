import org.scalatest._

import galileo.environment.Environment
import galileo.expr.{Number,Power,Product,Sum,Variable}
import galileo.exprhandler._
import galileo.parser._
import galileo.manipulate.{Expand,Simplify}

class SumTest extends FunSuite {
	val parser = new Parser()
  	val handler = new ExprHandler
	import parser.{ Success, NoSuccess }

	val va = Variable( "a" )
	val vb = Variable( "b" )
	val vc = Variable( "c" )
	test( "flatten") {
		val s = Sum( va, Sum( vb, vc ) )
		assert( s.terms.size == 2 )
		assert( s.flatTerms.size == 3 )
		s.visit() match {
			case ls:Sum => assert( ls.terms.size == 3 )
			case _ => fail( "Should be a sum")
		}
		//assert( p.visit().factors.size == 3 )
	}

	//   2*(2*c^2)*s^(-2) 
	// + 2*(-2*s^2*s^(-2)
	// - 4*c^2*s^-2
	// == 4
	val s = Sum(
		Product(
			Number(2.0),
			Product(Number(2.0),Power(Variable("c"),Number(2.0))),
			Power(Variable("s"),Number(-2.0))
		),
		// -4
		Product(
			Number(2.0),
			Product(Number(-2.0),Power(Variable("s"),Number(2.0))),
			Power(Variable("s"),Number(-2.0))
		),
		Product(
			Number(-4.0),
			Power(Variable("c"),Number(2.0)),
			Power(Variable("s"),Number(-2.0))
		)
	)

	test( "Sum of products" ) {
		val s1 = s.terms(1)
		assert(s1.visit() == Number( -4 ) )	
		assert(s.visit().toString == "-4.0")
	}
}
