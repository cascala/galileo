package galileo.expr

import galileo.environment.Environment

case class Max( exprs:Expr* ) extends Expr {
	override def eval() = Max( exprs.map( expr => expr.eval() ):_* ).visit()

	override def visit(env:Option[Environment]) = exprs.map( expr => expr.visit( env ) ).to(List) match {
		case Nil => this
		case e :: Nil => e // Max of list with one entry
		case Number( a ) :: Number( b ) :: Nil => Number( Math.max( a, b ) )
		case Fraction( Number( a ), Number( b ) ) :: Fraction( Number( c ), Number( d ) ) :: Nil if ( a * d  > b * c ) => Fraction( Number( a ), Number( b ) )
		case Fraction( Number( a ), Number( b ) ) :: Fraction( Number( c ), Number( d ) ) :: Nil => Fraction( Number( c ), Number( d ) )
		case Number( a ) :: Fraction( Number( b ), Number( c ) ) :: Nil if( a * c > b ) => Number( a )
		case Number( a ) :: Fraction( Number( b ), Number( c ) ) :: Nil => Fraction( Number( b ), Number( c ) )
		case Fraction( Number( a ), Number( b ) ) :: Number( c ) :: Nil if( b * c > a ) => Number( c )
		case Fraction( Number( a ), Number( b ) ) :: Number( c ) :: Nil => Fraction( Number( a ), Number( b ) )
		case Number( a ) :: Number( b ) :: tail => Max( Number( Math.max( a, b ) ), Max( tail:_* ).visit() ).visit()
		case e :: Number( a ) :: tail => Max( Number( a ), Max( tail:_* ).visit(), e ).visit()
		case Number( a ) :: b :: Number( c ) :: tail => Max( Number( Math.max( a, c ) ), b, Max( tail:_* ).visit() ).visit()
		case s => { println( "fell" ); Max( s:_* ) }// cast fron Seq to ellipsis
	}
	override def toString() = "max(" + exprs.mkString( "," ) + ")"
	override def info(env:Option[Environment]=None):String = "Max(" + exprs.mkString(",") + ")"
	def variables:List[Variable] = exprs.to(List).flatMap( expr => expr.variables )
}

case class Abs( e:Expr ) extends Expr {
	override def visit( env:Option[Environment]) = e.visit( env ) match {
		case Number( a ) if a < 0 => Number( -a )
		case Number( a ) => Number( a )
		case Fraction( a, b ) => Fraction( Abs( a ), Abs( b ) )
		case s:Expr => Abs( s )
	}
	override def toString() = "abs(" + e.toString() + ")"
	override def toStringWithSign() = "+" + toString() //if( value < 0 ) value.toString else "+" + value.toString()

	override def info(env:Option[Environment]=None):String = "Abs(" + e.toString() + ")"

	def variables:List[Variable] = e.variables
}
