import org.scalatest._

import util.Random
import util.Random.nextDouble
import scala.collection.mutable.ListBuffer

//import galileo.complex._
//import galileo.environment._
import galileo.expr._
//import galileo.exprhandler._
import galileo.parser._
import galileo.proof._

class Proof001 extends FunSuite {
	val expectedTruth = Map[String,Proof.Status](
		"a=a" -> Proof.Proven,
		//"a=-1*a" -> Proof.Disproved,
		"a=1*a" -> Proof.Proven,
		"0=b-b" -> Proof.Proven,
		//"a=0*a" -> Proof.Disproved, // todo, conditionally Proof.Proven
		"4=4" -> Proof.Proven,
		"5-1=3+1" -> Proof.Proven,
		"a*2=2*a" -> Proof.Proven,
		"3+2*a+4*b=b*4+a*2+3" -> Proof.Proven,
		//"3*(x+y+1)=3+3*y+3*x" -> Proof.Proven,
		"(y+1+x)+(x+y+1)=2*y+2+x*2" -> Proof.Proven,
		"1/x=x^(-1)" -> Proof.Proven,
		//"1/x/x=x^(-2)" -> Proof.Proven,
		"1/x^(-2)=x^2" -> Proof.Proven,
		"1=1" -> Proof.Proven	
	)

	test( "expectedTruth") {
		expectedTruth foreach { case (in:String, out:Proof.Status) => {
			val parser = new Parser()
			import parser.{ Success, NoSuccess }

			parser.parse( "prove(" + in + ")" ) match {
				case Success(expressions,_) => {
						assert( expressions.size == 1 )
						for( expression <- expressions )
						{
							val statement = expression.visit()
							statement match {
								case p:Proof => assert( p.status == out, "Expected " + out + " for " + in + ", but observed " + p.status )
								case s => fail( "Unexpected statement: " + s )
							}
						}			
					}
				case err: NoSuccess => fail( "Failure for " + in + ", " + out )
			}
		} }	
	}  	
}
