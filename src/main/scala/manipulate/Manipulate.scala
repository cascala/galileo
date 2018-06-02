package galileo.manipulate

import galileo.environment.Environment
import galileo.constants._
import galileo.expr._

// functions like:
// * expand
// * simplify
// * factor
// * combine

trait Manipulate {
	val expr:Expr
	def variables:List[Variable] = expr.variables
}

// expand products
// expand((a+b)*(a-b))=a^2-b^2
// Note: expand a^2 -> a^2 unless a is a sum or product
case class Expand(expr:Expr) extends Expr with Manipulate {
	override def visit( env:Option[Environment]=None):Expr = expr.visit(env).expand
	def info(env:Option[Environment]=None):String = "expand(" + expr.info(env) + ")"
}

// for now, only simplify fractions by removing common factors in numerator and denominator
case class Simplify(expr:Expr) extends Expr with Manipulate {      									 
	override def visit(env:Option[Environment]=None):Expr = {
		val simplifier = new ClosedFormSimplifier	
		simplifier.simplify( expr.visit(env) )
	}

	def info(env:Option[Environment]=None):String = "Simplify(" + expr.info(env) + ")"
}


case class Complexity(expr:Expr) extends Expr with Manipulate {      									 
	override def visit(env:Option[Environment]=None):Expr = {
		val simplifier = new ComplexityMinimizingSimplifier	
		Number( simplifier.complexity( expr.visit(env) ) )
	}

	def info(env:Option[Environment]=None):String = "Simplify(" + expr.info(env) + ")"
}

case class SimplifyMin(expr:Expr) extends Expr with Manipulate {      									
	override def visit(env:Option[Environment]=None):Expr = {
		val simplifier = new ComplexityMinimizingSimplifier
		simplifier.simplify( expr.visit(env) )
	}

	def info(env:Option[Environment]=None):String = "SimplifyMin(" + expr.info(env) + ")"
}

// factor 4*a*b+5*a*b -> 9*a*b
case class Factor(expr:Expr) extends Expr with Manipulate {
	override def visit( env:Option[Environment]=None):Expr = {
		//println( "factor(" + expr.visit() + ")" )
		expr.visit(env).factor.visit()
		//expr.factor
	}
	def info(env:Option[Environment]=None):String = "factor(" + expr.info(env) + ")"
}
