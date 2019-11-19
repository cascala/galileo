package galileo.rand

import util.Random
import util.Random.nextDouble

import galileo.environment.Environment
import galileo.expr.{ErrorExpr,Expr,Number,Statement}
import galileo.linalg.DenseMatrix


object Rand{
	val rng = new Random()

	def apply(n:Int):DenseMatrix = this.apply(n,n)

	def apply( nr:Int, nc:Int ) = {
		var rows:List[List[Expr]] = List() 
		for( i <- 0 until nr ) {
			var row:List[Expr] = List()
			for( j <- 0 until nc )
				row = row :+ Number( rng.nextDouble() )

			rows = rows :+ row	
		}
		DenseMatrix( rows )
	}
}

case class Rand(exprs:Expr*) extends Expr with Statement {
	def info(env:Option[Environment]=None) = "Rand(" + exprs.map( e => e.info(env)).mkString(",") + ")"
	override def visit(env:Option[Environment]) = exprs.map( expr => expr.visit( env ) ).to(List) match {
		case Number( d ) :: Nil if ( d%1 == 0 ) => Rand( d.toInt )
		case Number( d ) :: Nil => ErrorExpr( "rand(arg) only works for integer values of arg" )
		case Number( d1 ) :: Number( d2 ) :: Nil if( d1%1 == 0 && d2%1== 0 ) => Rand( d1.toInt, d2.toInt )
		case Number( d1 ) :: Number( d2 ) :: Nil => ErrorExpr( "rand(arg1,arg2) only works for integer values of arg1 and arg2" )

		case e => ErrorExpr( "Incorrect arguments in rand( " + e.map( expr => expr.info() ).mkString(",") + ")" ) 
	}
}
/*
case class Rand2(e1:Expr,e2:Expr) extends Expr {
	def info = "Rand2(" + e1 + )"
	override def visit(env:Option[Environment]) = expr.visit( env ) match {
		case Number( d ) if ( d%1 == 0 ) => Rand( d.toInt )
		case _ => ErrorExpr( "None" )
	}
}
*/
