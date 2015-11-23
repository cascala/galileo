package galileo.manipulate

import galileo.environment.Environment
import galileo.expr.{Expr,Fraction}

// functions like:
// * expand
// * simplify
// * factor
// * combine

// expand products
// expand((a+b)*(a-b))=a^2-b^2
// Note: expand a^2 -> a^2 unless a is a sum or product
case class Expand(expr:Expr) extends Expr {
	override def visit( env:Option[Environment]=None):Expr = expr.visit(env).expand
	def info(env:Option[Environment]=None):String = "expand(" + expr.info(env) + ")"
}

// for now, only simplify fractions by removing common factors in numerator and denominator
case class Simplify(expr:Expr) extends Expr {      									  // for now, add .visit add end
	override def visit( env:Option[Environment]=None):Expr = {
		//println( "Info, in Simplify:" + expr.visit(env).expand.simplify.visit().info() )
		expr.visit(env).expand.simplify.visit()
	}
	def info(env:Option[Environment]=None):String = "simplify(" + expr.info(env) + ")"
}

// factor 4*a*b+5*a*b -> 9*a*b
case class Factor(expr:Expr) extends Expr {
	override def visit( env:Option[Environment]=None):Expr = {
		//println( "factor(" + expr.visit() + ")" )
		expr.visit(env).factor.visit()
		//expr.factor
	}
	def info(env:Option[Environment]=None):String = "factor(" + expr.info(env) + ")"
}
