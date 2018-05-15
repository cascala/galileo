import galileo.complex._
import galileo.constants._
import galileo.environment._
import galileo.expr._
import galileo.exprhandler._
import galileo.linalg.{DenseMatrix,DiagMatrix,OnesMatrix}
import galileo.parser._

import org.scalatest._

import util.Random
import util.Random.nextDouble

class ParserTest extends FunSuite {
	val parser = new Parser()
  	val handler = new ExprHandler 
  	val genv = new Environment( None ) // Global env
  	genv.set( "pi", new ConstantPi() )
  	genv.set( "e", new ConstantE )
  	genv.set( "j", new ConstantJ )
  	genv.set( "i", new ConstantJ )
  	val rng = new Random()

	var a:Double = 0.0
	var b:Double = 0.0
	var e:Expr = Number(0)
	
	a = 10 * ( rng.nextDouble() - 0.5 ) + 10;
	b = 10 * ( rng.nextDouble() - 0.5 ) + 10;

	val senv = new Environment( Some( genv ) )
  	senv.set( "a", Number( a ) )
  	senv.set( "b", Number( b ) )
	
  	val expectedDouble = Map[String,Double](
  		"a" -> a,
  		"b" -> b, 
  		"a+b" -> { a + b }, //a + b,
  		"a*b" -> a * b,
  		"a^b" -> math.pow(a,b),

  		"tan(0)" -> 0,	
  		"cos(pi)" -> -1,
  		"cos(-1*pi)" -> -1,
  		"cos(3*pi)" -> -1,
  		"cos(6*pi)" -> 1,
  		"A=[4 5 6;6 5 4;7 8 7];norm(A-A,1)" -> 0,
		"A=[4 5 6;6 5 4;7 8 7];norm(2*A-A*2,1)" -> 0,
		"A=[4 5 6;6 5 4;7 8 7];norm(inv(A)*A-eye(3),1)" -> 0,
		"A=[4 5 6;6 5 4;7 8 7];norm(A*inv(A)-eye(3),1)" -> 0,


		"0" -> 0
  	)

  	val expectedExpr = Map[String,Expr](
  		"unknown" -> Variable( "unknown" ),
  		"j" -> Complex( Number( 0 ), Number( 1 ) ),
  		"j*j" -> Number( -1 ),
  		"j*i" -> Number( -1 ),
  		"j^6" -> Number( -1 ),
		"j" -> Complex( Number( 0 ), Number( 1 ) ),
		"j^3" -> Complex( Number( 0 ), Number( -1 ) ),
		"j^7" -> Complex( Number( 0 ), Number( -1 ) ),
		"j^11" -> Complex( Number( 0 ), Number( -1 ) ),
		"-12/3" -> Number( -4 ),
		"12/(-3)" -> Number( -4 ),
		"(-12)/(-3)" -> Number( 4 ),
		"1/3+2/6" -> Fraction( Number( 2 ), Number( 3 ) ),
		"7*1/6" -> Fraction( Number( 7 ), Number( 6 ) ),
		"log(e)" -> Number( 1 ),
		"log(e^3)" -> Number( 3 ),
		"log(e^5*e^4)" -> Number( 9 ),
		"eye(1)*2" -> DiagMatrix( 1, List( Number( 2 ) ) ),
		"eye(2,4)*3" -> DiagMatrix( 4, List( Number( 3 ), Number( 3 ) ) ),
		"[3/4 5]" -> DenseMatrix( List( List( Fraction( Number( 3 ), Number( 4 ) ), Number( 5 ) ) ) ),

		"ones(2,1)" -> OnesMatrix( 2, 1, Number( 1 ) ),
		"zeros(2,3)" -> OnesMatrix( 2, 3, Number( 0 ) ),

		"1" -> Number( 1 )
  		//"cos(pi)" -> Number( 0 )
  	)

	// Note that the compiler can actually determine the type
	// so, this would work as well
	// val expectedEnv = Map(
	val expectedEnv = Map[String,Map[String,Option[Expr]]] (
		"a=1" -> Map( "a" -> Some( Number(1) ) ),
		"b=2" -> Map( "b" -> Some( Number(2) ) ),
		"a=1;b=2;c=a+b" -> Map( "c" -> Some( Number(3) ) )
	)

  	val expectedString = Map[String,String](
  		"1" -> "1.0",
  		"1+10+u+1" -> "12.0+u",
		"8*y*u*y*u*y^5*y*u*u^6*r*y*7*y" -> "56.0*r*u^9.0*y^10.0",
		"9+x" -> "x+9.0",
		"va+va" -> "2.0*va",
		"va+2*va" -> "3.0*va",
		"2*va+3*va" -> "5.0*va",
		"4*va-5*va" -> "(-1.0)*va",
		"cos(x+1)" -> "cos(x+1.0)",
		"cos(x*1)" -> "cos(x)",
		"cos(x*4)" -> "cos(4.0*x)",
		"2.0*sin(psi)^2.0+sin(psi)^2.0" -> "3.0*sin(psi)^2.0",
		"1+10+u+1" -> "u+12.0",
		"1+10-u+1" -> "(-1.0)*u+12.0",
		"-1*(5+5)+15" -> "5.0",
		"va*vb+8*va*vb" -> "9.0*va*vb",
		"va*vb+8*va*vb-va*vb" -> "8.0*va*vb",
		"va+vb-vb" -> "va",
		"-1*(va+vb)+vc" -> "(-1.0)*(va+vb)+vc", // a, b and c were set in env above
		"-1*(va+vb)-vc" -> "(-1.0)*(va+vb)-1.0*vc",
		"cos(x*7)" -> "cos(7.0*x)",
		"acos(cos(y))" -> "y",
		"[-2.3]" -> "-2.3",
		"[5/6]" -> "5.0/6.0",
		"-(zzzz)" -> "(-1.0)*zzzz",
  		"-[5 6]" -> "-5.0	-6.0",
		"norm([1 3;5 (-6)],1)" -> "9.0", // [ 1 4; 1 -6] is interpreted as [ 1 4; 1-6] == [ 1 4; -5 ], so not a square matrix
		"norm([3 6;9 (-1)]/2,1)" -> "6.0",
		"A=[4 5 6;6 5 4;7 8 7];norm(A-A,1)" -> "0.0",
		"A=[4 5 6;6 5 4;7 8 7];norm(2*A-A*2,1)" -> "0.0",	
		"A=[4 5 6;6 5 4;7 8 7];norm(inv(A)*A-eye(3),1)" -> "0.0",
		"A=[4 5 6;6 5 4;7 8 7];norm(A*inv(A)-eye(3),1)" -> "0.0",
		"deriv(x,x)" -> "1.0",
		"1&&0" -> "false",
		"(1&&0)||true" -> "true",
		"and(1,0||1)" -> "true",
		"or(1||0,false)" -> "true",
		"xor(1,1)" -> "false",
		"xor(0,true)" -> "true",
		"xor(0,0)" -> "false",		
		"0" -> "0.0"
  	)

	import parser.{ Success, NoSuccess }

	test( "expectedDouble") {
		expectedDouble foreach { case (s:String, d:Double) => {
				parser.parse( s ) match {
					case Success(expressions,_) => {
						assert( expressions.size > 0 )
						var r:String = ""
						for( expression <- expressions )
							r = handler.eval( senv, expression)		
						assert( math.abs( r.toDouble - d ) < 1E-10, s)
					}
					case err: NoSuccess   => fail( "Failure for " + s + ", " + d )
				}
			}
		}
	}

	test( "expectedExpr") {
		expectedExpr foreach { case (s:String, e:Expr) => {
			parser.parse( s ) match {
					case Success(expressions,_) => {
						assert( expressions.size == 1 )
						val expression = expressions( 0 )
						val r1 = handler.eval( senv, expression)
						val r2 = handler.eval( senv, e )		
						assert( r1 == r2 )
					}
					case err: NoSuccess   => fail( "Failure for " + s + ", " + e )
				}
			}
		}
	}

	test( "expectedEnv") {
		expectedEnv foreach { 
			case (s:String, e:Map[String,Option[Expr]]) => {
				var env = new Environment
				parser.toExpr( env, s ) match {
					case Some( l ) => {
						for( (k,v) <- e )
							assert( env.get( k ) == v )
					}
					case None => fail( "Failure for " + s + ", " + e )
				}
			}
		}
	}

	test( "expectedString") {
		expectedString foreach { case (in:String, out:String) => {
			parser.parse( in ) match {
				case Success(expressions,_) => {
						assert( expressions.size > 0 )
						var r = ""
						for( expression <- expressions )
						{
							r = handler.eval( senv, expression)		
							assert( r != "" )
						}
						assert( out == r )
					}
				case err: NoSuccess   => fail( "Failure for " + in + ", " + out )
			}
		} }	
	}  	//assert( 1 == 1 )
}
