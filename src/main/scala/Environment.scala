package galileo.environment

import galileo.complex._
import galileo.constants._
import galileo.expr._
import galileo.linalg._
import galileo.logic._

import scala.collection.mutable.Map

// Nested environment structure
// Global, session, local variables etc.
class Environment(parent:Option[Environment]) {
	private var variables = Map[String,Expr]() // should be pointer to subset of Expr? 
	def clear:Unit  = {
		variables = variables.empty
	} 
	def get(name:String):Option[Expr] = {
		if( variables.contains( name ) )
			Some( variables( name ) )
		else {
			parent match{
				case Some(p) => p.get(name)
				case None => return None
			}
		}
	}

	def getWithout(name:String):Environment = {
		val cp = new Environment( this.parent )
		var vars = this.variables
		vars.remove( name )
		cp.variables = vars
		cp
	}

	//private def setVariables 

/*

	def getWithout(name:String):Environment = {
		val cp = this.copy()
		cp.
	}*/

	def set(key:String,value:Expr):Unit = {
		variables(key) = value
	}

	def allVariables:Map[String,Expr] = parent match {
		case Some( p ) => p.allVariables ++ variables
		case None => variables
	}

	override def toString() = {
		//var s = variables.foreach()
		def toType( v:Expr ):String = v match{
			case Number(_) => "Scalar"
			case Bool(_) => "Boolean"
			case m:Matrix => "Matrix ( " + m.numRows + " x " + m.numCols + " )"
			/*case m:DiagMatrix => "Matrix ( " + m.numRows + " x " + m.numCols + " )"
			case m:LowerTriangularMatrix => "Matrix ( " + m.numRows + " x " + m.numCols + " )"
			case m:UpperTriangularMatrix => "Matrix ( " + m.numRows + " x " + m.numCols + " )"
			*/case Variable(_) => "Variable"
			case Complex(_,_) => "Complex Expression"
			case c:Constant => "Constant" + " " + toType( c.value )
			case p:Product => "Product expression"
			//case ps:ProductSeries => "ProductSeries expression"
			case _ => "Expression" 
		}
		val terms = allVariables map { case (k,v) => k + ":\t" + toType( v ) } 
		"Defined variables:\n" + ( terms mkString "\n" ) //_._1 + "=" + _._2.toString() )
		//terms mkString "\n"
	}
}
