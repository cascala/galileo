package galileo.expr

import galileo.environment.Environment

case class Info(e:Expr) extends Expr with Statement {
	override def visit(env:Option[Environment] = None ) = StringExpr( e.info(env) )
	def info(env:Option[Environment]=None) = "Info(" + e + ")"
}
