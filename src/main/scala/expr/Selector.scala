package galileo.expr

import galileo.environment.Environment
import galileo.selectable.Selectable

// to select from a matrix, vector, tensor...
case class Selector( selectee:Expr,indices:Expr*) extends Expr {
	def info(env:Option[Environment]=None) = "Selector(" + selectee + "[" + indices.mkString( "," ) + "])"
	override def visit(env:Option[Environment]=None):Expr = {
		val sel = selectee.visit( env )
		val ind = indices.map( index => index.visit( env )).to[List]
		sel match { 
			case s:Selectable => s.select( ind)
			case e:Expr => ErrorExpr( "Can not select from " + e.info())
		}
	}
}