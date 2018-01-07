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

// Used for loading and runninf files
// Sample use 'load(examples/a.gg)'
case class Load(filename:String) extends Expr with Statement {
	def info(env:Option[Environment]=None) = "Load(" + filename + ")"
	
	override def visit( env:Option[Environment] ):Expr = {
		val lines = Source.fromFile(filename).getLines
		val p = new Parser()
		var l:Expr = new NilExpr() // to return the last expression in the file
		for( line <- lines ) {
			p.parse( line ) match {
				case p.Success(expressions,_) => { 
				for(expression <- expressions)
					l = expression.visit(env) 
				}
				case err => ErrorExpr( "Failure to parse " + filename + err  )
			}
		}
		l
	} // visit
}
