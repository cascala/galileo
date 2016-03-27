package galileo.selectable

import galileo.expr.Expr

trait Selectable {
	def select(indices:List[Expr]):Expr
}