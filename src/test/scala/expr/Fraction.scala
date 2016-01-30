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
	val vc = Variable( "c" )
	val vd = Variable( "d" )

	val expectedExtractions = List(
		( Fraction( va, Square( vb ) ),                     Fraction( Number( 1 ), vb ),            "a/b"   ),
		( Fraction( Product( va, vb ), Product( vc, vd ) ), Fraction( Number( 1 ), vd ),            "a*b/c" ),
		( Fraction( va, Sum( vb, vc ) ),                    Fraction( Number( 1 ), Sum( vb, vc ) ), "a/1.0"     )
	)

	test( "extractFactor") {
		for( expectedExtraction <- expectedExtractions ) {
			val f = expectedExtraction._1
			val e = expectedExtraction._2
			val s = expectedExtraction._3
			f.extractFactor( e ) match {
				case Some( r ) => assert( r.toString == s )
				case None => { 
					info( "f.numerator.extractFactor():" + f.numerator.extractFactor( e.numerator ) )
					info( "f.denominator.extractFactor():" + f.denominator.extractFactor( e.denominator ) )
					fail( "expected:" + s )
				}
			}
		}
	}
}
