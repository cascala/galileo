package galileo.expr

object RootN{
	def apply( operand:Expr, N:Expr ) = Power( operand, Fraction( Number( 1 ), N ) )
}

object Sqrt{
	def apply(operand:Expr) = Power( operand, Fraction( Number( 1 ), Number( 2 ) ) )
}
