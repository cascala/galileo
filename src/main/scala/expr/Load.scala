package galileo.expr

import galileo.environment.Environment
import galileo.exprhandler.ExprHandler
import galileo.parser._

import scala.io.Source

//import scala.util.parsing.combinator.JavaTokenParsers
//import scala.util.parsing.combinator.ImplicitConversions // really useful...
//import java.io.FileNotFoundException
//import java.io.IOException
//
//import scala.util.parsing.combinator.JavaTokenParsers.{Success,NoSuccess}

case class Load(filename:String) extends Expr {
	def info(env:Option[Environment]=None) = "Load(" + filename + ")"
	
	def expressions:List[Expr] = {
		//var contents = "a=10;b=11;c=a+b;c"
		val contents = Source.fromFile(filename).getLines.mkString
		// parse this (or should we send the contents to the command window?)
		val p = new Parser()
		p.parse( contents ) match {
			case p.Success(expressions,_) => { 
				/*env match { // Handle the expressions }
					case Some( e ) => h.eval( e, ExprArray( expressions:_* ) ) //.foreach( expression => h.eval( e, expression ) )
					case None => h.eval( new Environment( None ), ExprArray( expressions:_* ) )
					//expressions.foreach( expression => h.eval( new Environment( None ), expression ) ) //ErrorExpr( "Internal error" ) // handler.eval( )
				}
				*/
				expressions
			}
			case err => List( ErrorExpr( "Failure to parse " + contents + err  ) )
		}
	}
	

	override def visit( env:Option[Environment] ) = {
		var p = new Parser()
		//var h = new ExprHandler()
		
		//import parser.NoSuccess // this does not compile... argh
//import p.Success
		//var contents = "a=10;b=11;c=a+b"
		val contents = Source.fromFile(filename).getLines.mkString
		// parse this (or should we send the contents to the command window?)
		p.parse( contents ) match {
			case p.Success(expressions,_) => { 
				/*env match { // Handle the expressions }
					case Some( e ) => h.eval( e, ExprArray( expressions:_* ) ) //.foreach( expression => h.eval( e, expression ) )
					case None => h.eval( new Environment( None ), ExprArray( expressions:_* ) )
					//expressions.foreach( expression => h.eval( new Environment( None ), expression ) ) //ErrorExpr( "Internal error" ) // handler.eval( )
				}
				*/
				ExprArray( expressions:_* )
			}
			case err => ErrorExpr( "Failure to parse " + contents + err  )
		}
		
		//Number( 1 )
		//Expr
	} // visit


}
