package galileo.proof

import galileo.expr.Expr

case class Rule(description:String, result:Relation) {
	override def toString() = "Applying the rule \"" + description + "\" results in " + result
	def flipLeftAndRight = Rule( description, result.flipLeftAndRight )
}

case class Conversion( description:String, expr:Expr ) {}

// Goal is to find Truth

// Rule( 0 )
// If(equality(l, r) ) then equality ( l + x, r + x )
// If( equality( l, l )) then Truth
// If( equality( l, l + x )) and x != 0 then Falsitude
// Rule( )
