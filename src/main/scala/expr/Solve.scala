package galileo.solve

import galileo.expr.{ErrorExpr,Expr}
import galileo.environment.Environment
import galileo.linalg.{Matrix,DenseMatrix}

case class Solve( lhs:Expr, rhs:Expr ) extends Expr {
	override def visit( env:Option[Environment]=None):Expr = ( lhs.visit( env ), rhs.visit( env ) ) match {
  		case (l:DenseMatrix, r:DenseMatrix ) if ( l.numRows == l.numCols && l.numRows == r.numRows ) => l.solve( r )
		case (l:Matrix, r:DenseMatrix ) if ( l.numRows == l.numCols && l.numRows == r.numRows ) => l.solve( r )
  		case (l:Matrix, r:Matrix ) if ( l.numRows == l.numCols && l.numRows == r.numRows ) => l.solve( r.toDenseMatrix )
  		case _ => ErrorExpr( "Can't solve this matrix\\rhs combination" )// todo clear error message, not solvable
  	} 
  	def info(env:Option[Environment]=None) = "Solve(" + lhs + "," + rhs + ")"
}
