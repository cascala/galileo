import org.scalatest._

import galileo.environment.Environment
import galileo.expr.{Number,Product,Sum,Variable}
import galileo.exprhandler._
import galileo.parser._
import galileo.manipulate.{Expand,Simplify}

class SimplifyTest extends FunSuite {
	val parser = new Parser()
  	val handler = new ExprHandler
	import parser.{ Success, NoSuccess }

	val expected = Map[String,String](
		"y^5*x^3*t/t^2/x" -> "x^2.0*y^5.0/t",
		"sin(x)^2/(6*sin(x)*x)" -> "sin(x)/(6.0*x)",
		"a^2/a^4" -> "1.0/a^2.0",
		"a^4/a^2" -> "a^2.0",
		"(2*a+2*b)/2" -> "a+b",
		"(2*a*b+3*a*c)/a" -> "2.0*b+3.0*c",
		"1*(2*a*b+3*a*c)/a" -> "2.0*b+3.0*c",
		"-1*(2*a*b+3*a*c)/a" -> "(-2.0)*b+(-3.0)*c",
		"4.0*(sin(theta)^5.0+2.0*sin(theta)^6.0)/sin(theta)" -> "4.0*sin(theta)^4.0+8.0*sin(theta)^5.0",
		"4.0*(sin(theta)^5.0+2.0*sin(theta)^6.0)/sin(theta)^2" -> "4.0*sin(theta)^3.0+8.0*sin(theta)^4.0",
		"2*((-2)*s^2+2*c^2)*s^(-2)+(-4)*c^2*s^(-2)" -> "-4.0",
		"2*s^(-2)*s^2" -> "2.0",
		"-4*a*b+2*a*b" -> "(-2.0)*a*b",
		"cos(a)^2+sin(a)^2" -> "1.0",
		"d*cos(a)^2+d*sin(a)^2" -> "d",
		"(-1*a*b)/(-1*c*d)" -> "a*b/(c*d)",
		//"c/a+b*c^2.0/(a^2.0*d+(-1.0)*a*b*c)+(-1.0)*c*d/(a*d+(-1.0)*b*c)" -> "0.0",
		//"b/a+b^2.0*c/(a^2.0*d+(-1.0)*a*b*c)+(-1.0)*b*d/(a*d+(-1.0)*b*c)" -> "0.0",
		"8*y/4"-> "2.0*y",
		//"a/(2*b)-a/b+a/(2*b)" -> "0.0", // can be fixed with better sorting...
		//"b+b/c" -> "b*(1+1/c)",
		//"2*a+2*b" -> "2.0*(a+b)",
		//"-2*a+b" -> "-2.0*(a-b)",
		"1" -> "1.0"	
	)
	val genv = new Environment( None ) 

	test( "simplify") {
		expected foreach { case (in:String, out:String) => {
			parser.parse( "simplify(" + in + ")" ) match { // make this just 'visit?'
				case Success(expressions,_) => {
						assert( expressions.size == 1 )
						//println( "info" + expressions(0).info() )
						val r = handler.eval( genv, expressions(0))		
						assert( r != "" )
						assert( out == r )
					}
				case err: NoSuccess   => fail( "Failure for " + in + ", " + out )
			}
		} }
	}
}

class FactorTest extends FunSuite {
	val parser = new Parser()
  	val handler = new ExprHandler
	import parser.{ Success, NoSuccess }

	val expected = Map[String,String](
		"-4*a*b+2*a*b" -> "(-2.0)*a*b",
		"a*b+a*b+2*a*b" -> "4.0*a*b",
		//"c*a*b+b+a*b+d*b" -> "b*(a*(c+1.0)+d+1.0)",
		//"a*b*c+a*b+a*d" -> "a*(b*(c+1.0)+d)",
		"2*a+2*b" -> "2.0*(a+b)",
		"-2*a+2.0*b" -> "(-2.0)*(a+(-1.0)*b)",
		"7.0*c*u^2.0+d*u^2.0" -> "(7.0*c+d)*u^2.0",
		"u*(u*d+u*7)" -> "(d+7.0)*u^2.0",
		//"1.0/a+b*c/(a^2.0*d+(-1.0)*a*b*c)" -> "(b*c/(a*d+(-1.0)*b*c)+1.0)/a",
		//"b+b/c" -> "b*(1.0/c+1.0)",
		//"a/b+c/b^2" -> "(a+c/b)/b",
		"1+a+a^2" -> "a*(a+1.0)+1.0",
		//"1+1/a+1/a^2" -> "(1.0/a+1.0)/a+1.0",
		//"a/(b*c)+d/(b*e)" -> "(a/c+d/e)/b",
		//"b/(a*(a+b))+c/(a+b)" -> "(b/a+c)/(a+b)",
		//"a*b*c*(a+b)+d*e*f*(a+b)" -> "(a+b)*(a*b*c+d*e*f)",
		//"(a+b)+d*e*f*(a+b)" -> "",
		//"(b*c/(a*(a*d+(-1.0)*b*c))+(-1.0)*d/(a*d+(-1.0)*b*c))*c" -> "",
		"1" -> "1.0"	
	)
	val genv = new Environment( None ) 

	test( "factor") {
		expected foreach { case (in:String, out:String) => {
			parser.parse( "factor(" + in + ")" ) match {
				case Success(expressions,_) => {
						assert( expressions.size == 1 )
						val r = handler.eval( genv, expressions(0))		
						assert( r != "" )
						assert( out == r )
					}
				case err: NoSuccess   => fail( "Failure for " + in + ", " + out )
			}
		} }
	}
}

class ExpandSimplifyTest extends FunSuite {
	val parser = new Parser()
  	val handler = new ExprHandler
	//import parser.{ Success, NoSuccess }

	val expected = Map[String,String](
		"a*(a+b)" -> "a*a+a*b",
		"(a+b)*(a+b)" -> "a*a+a*b+b*a+b*b",
		"(a+b)*(a-b)" -> "a*a+a*(-1.0)*b+b*a+b*(-1.0)*b",
		"1" -> "1.0"	
	)
	val genv = new Environment( None ) 

	test( "001" ) {
		import parser.{ Success, NoSuccess }
		expected foreach { case (in:String, out:String) => {
			parser.parse( "expand(" + in + ")" ) match {
				case Success(expressions,_) => {
						assert( expressions.size == 1 )
						val r = handler.eval( genv, expressions(0))		
						assert( r != "" )
						assert( out == r )
					}
				case err: NoSuccess   => fail( "Failure for " + in + ", " + out )
			}
		} }
	}
}

class ExpandTest extends FunSuite {
	val parser = new Parser()
  	val handler = new ExprHandler
	//import parser.{ Success, NoSuccess }

	val expected = Map[String,String](
		"a*(a+b)" -> "a*a+a*b",
		"(a+b)*(a+b)" -> "a*a+a*b+b*a+b*b",
		"(a+b)*(a-b)" -> "a*a+a*(-1.0)*b+b*a+b*(-1.0)*b",
		"1" -> "1.0"	
	)
	val genv = new Environment( None ) 

	test( "001") {
		import parser.{ Success, NoSuccess }
		expected foreach { case (in:String, out:String) => {
			parser.parse( "expand(" + in + ")" ) match {
				case Success(expressions,_) => {
						assert( expressions.size == 1 )
						val r = handler.eval( genv, expressions(0))		
						assert( r != "" )
						assert( out == r )
					}
				case err: NoSuccess   => fail( "Failure for " + in + ", " + out )
			}
		} }
	}
}

class extractFactorTest extends FunSuite {
	test( "001") {
		val n = Number( 3 )
		//info( "n.extractFactor(Number(3)): " + n.extractFactor(Number(3) ) )
		assert( n.extractFactor( Number(3) ) == Some( Number( 1 ) ) )
		val p = Product( Number( 3 ), Variable( "a") )
		//info( "p.extractFactor(Number(3)): " + p.extractFactor(Number(3) ) )
		val s = Sum( Product( Number( 3 ), Variable( "a" ) ), Product( Number( 3 ), Variable( "b" ), Variable( "a" ) ) )
		//info( "s.factors" + s.factors )
		assert( s.possibleFactors == List( s, Number( 3 ), Variable( "a" ) ) )
		assert( s.extractFactor( Variable( "a" ) ) == Some( Sum( Product(Number(3),Variable("b")), Number(3) ) ) )
	}
}
