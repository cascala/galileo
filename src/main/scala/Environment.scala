package galileo.environment

import galileo.complex._
import galileo.constants._
import galileo.expr._
import galileo.linalg._
import galileo.logic._
import galileo.tensor.{Metric,Tensor}

import scala.collection.mutable.Map

// Nested environment structure
// Global, session, local variables etc.
class Environment(parent:Option[Environment]) {
	// connvenient additional constructor
	def this() = this( None ) 

	// the main storage for all variables
	private var variables = Map[String,Expr]() // should be pointer to subset of Expr? 
	
	// reset the environment, but not the parent environment
	def clear:Unit  = {
		variables = variables.empty
	} 

	// get the value of a variable called name
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

	// return a new environment instance without the variable called name
	def getWithout(name:String):Environment = {
		val cp = new Environment( this.parent )
		var vars = this.variables
		vars.remove( name )
		cp.variables = vars
		cp
	}

	// set a single variable
	def set(key:String,value:Expr):Unit = {
		variables(key) = value
	}

	// return a map or all variables of this environment and its parent (recursively)
	def allVariables:Map[String,Expr] = parent match {
		case Some( p ) => p.allVariables ++ variables
		case None => variables
	}

	// string representation of all variables, used in the 'who' command
	override def toString() = {
		//var s = variables.foreach()
		def toType( v:Expr ):String = v match{
			case Number(_) => "Scalar"
			case Bool(_) => "Boolean"
			case m:Matrix => "Matrix:" + m.numRows + "(r)," + m.numCols + "(c)"
			/*case m:DiagMatrix => "Matrix ( " + m.numRows + " x " + m.numCols + " )"
			case m:LowerTriangularMatrix => "Matrix ( " + m.numRows + " x " + m.numCols + " )"
			case m:UpperTriangularMatrix => "Matrix ( " + m.numRows + " x " + m.numCols + " )"
			*/case Variable(_) => "Variable"
			case Complex(_,_) => "Complex Expression"
			case c:Constant => "Constant" + " " + toType( c.value )
			case p:Product => "Product expression"
			case m:Metric => "Metric:" + m.variables.mkString(",")
			case t:Tensor => "Tensor:" + t.indices.mkString(",")
			//case ps:ProductSeries => "ProductSeries expression"
			case _ => "Expression" 
		}
		val terms = allVariables map { case (k,v) => k + ":\t" + toType( v ) } 
		"Defined variables:\n" + ( terms mkString "\n" )
	}
}
