import org.scalatest._

import galileo.environment.Environment
import galileo.expr._
import galileo.exprhandler._
import galileo.parser._
import galileo.manipulate.{Expand,Simplify}
import galileo.trigonometry.CosF1

class ProductTest extends FunSuite {
	val parser = new Parser()
  	val handler = new ExprHandler
	import parser.{ Success, NoSuccess }

	val va = Variable( "a" )
	val vb = Variable( "b" )
	val vc = Variable( "c" )
	val n2 = Number( 2 )
	val cs = Square( CosF1( vc ) )
	test( "flatten") {
		val p = Product( va, Product( vb, vc ) )
		assert( p.factors.size == 2 )
		assert( p.flatFactors.size == 3 )
		p.visit() match {
			case pl:Product => assert( pl.factors.size == 3 )
			case _ => fail( "Should be a product")
		}
		assert( p.expressify( List( va, vb ) ) == Product( va, vb ) )

		val p2 = Product( n2, Product( n2, cs ) )
		assert( p2.factors.size == 2 )
		assert( p2.flatFactors.size == 3 )
		p2.visit() match {
			case pl:Product => assert( pl.factors.size == 2 )
			case _ => fail( "Should be a product")
		}
		
		val p3 = Product(
			Number(2.0),
			Product(Number(-2.0),Power(Variable("s"),Number(2.0))),
			Power(Variable("s"),Number(-2.0))
		)
		assert( p3.factors.size == 3 )
		assert( p3.flatFactors.size == 4 )
		assert( p3.visit() == Number( -4 ) )
		//assert( p.visit().factors.size == 3 )
	}
}
