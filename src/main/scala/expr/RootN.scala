package galileo.expr

import galileo.constants.ConstantJ

object RootN{
	def apply( operand:Expr, N:Expr ) = Power( operand, Fraction( Number( 1 ), N ) )
}

object Sqrt{
	def apply(operand:Expr) = operand match {
		case Number( -1 ) => new ConstantJ()
		case _ => Power( operand, Fraction( Number( 1 ), Number( 2 ) ) )
	}
}
