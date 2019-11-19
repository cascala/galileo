package galileo.proof

import galileo.expr._

trait Relation {
	def rules(depth:Int):List[Rule]
	def flipLeftAndRight = this
}

case class Truth( description:String ) extends Relation {
	def rules(depth:Int) = List()
	override def toString = "Truth: " + description
}
case class Falsitude( description:String ) extends Relation {
	def rules(depth:Int) = List()
	override def toString = "Falsitude: " + description
}

case class Equality(left:Expr, right:Expr ) extends Relation {
	override def flipLeftAndRight = Equality( right, left )
	/*
	override def visit( env:Option[Environment]):Expr = {
		var steps:List[Rules] = List()
		// now, find that List of rules to apply

		/*
		(l.visit(env),r.visit(env)) match{
			case (a,b) if (a == b ) => 
		}
		*/
		// different methods
		// 1. labyrinth solve first, w backtrack, heuristics etc
		// 2. Genetic algorithm

	}
	*/
	// return all valid rules that could be applied/should be tried on this equality
	def rules(depth:Int):List[Rule] = {
		var cls:List[Conversion] = List( Conversion( "Identity", left ) )
		var crs:List[Conversion] = List( Conversion( "Identity", right ) )
		def handler( expr:Expr ):List[Conversion] = { /* println( "In handler, expr.info(): " + expr.info() );*/ expr } match {
			case p:Product => p.conversions( depth )
			case s:Sum => s.conversions( depth )
			case f:Fraction => f.conversions( depth )
			case p:Power => p.conversions( depth )
			case v:Variable => List()
			case n:Number => List()
			//case _ => List() //{ println( "No nothing matched for " + expr.info() ); List() }
		}
		//println( "handler(right)" + handler( right ) )
		cls = cls ++ handler( left )
		crs = crs ++ handler( right )
		//println( "cls: " + cls )
		//println( "crs: " + crs )

		var rv:List[Rule] = List()
		for( cl <- cls )
			for( cr <- crs )
			{
				if( cl.expr == cr.expr )
					rv = rv :+ Rule( "LHS rule: " + cl.description + ", RHS rule: " + cr.description, Truth( cl.expr.toString + "=" + cr.expr.toString ) )
				else
					rv = rv :+ Rule( "LHS rule: " + cl.description + ", RHS rule: " + cr.description, Equality( cl.expr, cr.expr ) )
			}
		//return rv	
//println( "RV HERE: " + rv )
		/*
		var rs:List[Expression] = List()
		// expressions are all kinds of modifications of expressions, applying distribution, factorization, changing order etc...
		rs = rs ++ left.conversions(10) // hardcoded depth for now
		rs = rs ++ right.conversions(10) // hardcoded depth for now
		// the engine will then figure out if the rules lead to a truth
		rs
*/

		/*
		def termRules( term:Expr ):(String,Expr) = {
			var r:List[Rule] = List()
			term match {
				case Product( s:Sum, a:Expr ) => ( "Distributing product of " + s + " and " + a, Sum( s.terms.map( t => Product( t, a ) ):_* ) )
				case _ => ( "", term )
			}
		}*/
		

		def termRules( term:Expr ):(String,Expr) = {
			var r:List[Rule] = List()
			term match {
				case Product( s:Sum, a:Expr ) => ( "Distributing product of " + s + " and " + a, Sum( s.terms.map( t => Product( t, a ) ):_* ) )
				case Product( a:Expr, s:Sum ) => ( "Distributing product of " + a + " and " + s, Sum( s.terms.map( t => Product( a, t ) ):_* ) )
				case p:Product => ( "Sorting factors in " + p, Product( p.flatFactors.sortWith( Product.sort ) ) )
				case _ => ( "", term )
			}
		}	
		/*
		Cleans up left side only
		*/
		def onesider( left:Expr, right:Expr ):List[Rule] = {
			var r:List[Rule] = List()
			//println( left )
			left match {
				//case p:Product => r = r ++ p.conversions.map( c -> Rule( "bla" ), Equality)
				case Sum( Number( a ), Number( b ) ) => r = r :+ Rule( "Executing sum " + left, Equality( Number( a + b ), right ) )
				// how to check the termrules actually did or suggeted any rules?
				case s:Sum => {
					val t = s.terms.map( term => termRules( term ) ) // array of tuples
					val descriptions = t.map( { case (_1,_2) => _1 } )
					if( !descriptions.flatten.isEmpty ){
						
						// How to check that anything actually happened?
 						r = r :+ Rule( 
							descriptions.mkString( "\n" ), 
							Equality( Sum( t.map( { case (_1,_2) => _2 } ):_* ), right )
						)
 					}
 					// all other simplifications inside a sum... (a-a->0 etc...)
 					r = r :+ Rule( "Sorting terms in " + s, Equality( Sum( s.flatTerms.sortWith( Sum.sort ) ), right ) ) 

 					// sort sum...
 					//r = r :+ Rule( "Sum simplifications", Equality( s.visit(), right ) )
				} 

				case Product( Number( a ), Number( b ) ) => r = r :+ Rule( "Executing product " + left, Equality( Number( a * b ), right ) )
				//case Product( Number( 1 ) :: tail :: Nil ) => r = r :+ Rule( "Removed multiplation with 1", Equality( Product( tail:_* ), right ) )
				//case Product
				case Product( s:Sum, b:Expr) => r = r :+ Rule( "Distributing product of " + s + " and " + b , Equality( Sum( s.terms.map( term => Product( term, b ) ):_* ), right ) )// distribute
				case n:Number => Nil
				case _ => left.visit() match {
					case n:Number => r = r :+ Rule( "Simplifying left hand side " + left + " to " + n, Equality( n, right ) )
					case _ => Nil
				}
			} 
			r
		}

		(left,right) match {
			//case Number(a), Number(b) if ( a == b ) => rv = rv :+ Rule( "Equality of numbers", Truth( a + "=" + b ) )
			case (a:LogF1,b:LogF1) => rv = rv :+ Rule( "Inverse of log on both sides", Equality( a.e, b.e ) ) 
			case (a:Expr,b:Expr) if ( a == b ) => rv = rv :+ Rule( "Equality of expressions", Truth( a.toString + "=" + b.toString ) )
			case (Product( a, b), Product( c, d) ) if ( a == c ) => rv = rv :+ Rule( "Removal of common factor " + a, Equality( b, d ) )
			case (a:Expr, Product( Number( -1 ), b:Expr ) ) if ( a == b ) => rv = rv :+ Rule( "Removal of -1", Equality( a , Number( 0 ) ) )
			case (Number(a), Number(b)) => rv = rv :+ Rule( "Inequality of numbers", Falsitude( a.toString + "!=" + b.toString ) )
			/*case (v:Variable, Number( 0 ) ) => rv = rv :+ Rule( "Conditional truth if " + v " equals 0", Conditional( 
				(Condition( Equality( v, Number( 0 ) ), Truth( v + "=" 0 ), 
				(Condition( InEquality( v, Number( 0 ) ), Equaltity( v, Number( 0 ) ) )
			) )
				*/
			case (Variable(a),Variable(b)) => rv = rv :+ Rule( "Inequality of variables", Falsitude( a + "!=" + b ) )
			case (Sum( a, b ), c:Expr) if ( a == c ) => rv = rv :+ Rule( "Removal of common term " + a, Equality( b, Number( 0 ) ) )
			case ( Fraction( Number( 1 ), a:Expr ), Power( b:Expr, Number( -1 ) ) ) if ( a == b ) => rv = rv :+ Rule( "1/x=x^(-1)", Truth( left.toString + "=" + right.toString ) )
			case ( Fraction( Number( 1 ), Power( a:Expr, e:Expr ) ), Power( b:Expr, f:Expr ) ) if ( a == b && e == -f ) => 
				rv = rv :+ Rule( "1/x^n=x^(-n)", Truth( left.toString + "=" + right.toString ) )
			//case (Number( a ), Sum( Number( b ), c ) ) if ( b == a ) => rv = rv :+ Rule( "Removal of common term " + a, Equality( Number( 0 ), c ) )
			//case (Number( a ), Sum( Number( b ), c ) ) if ( a > b ) => rv = rv :+ Rule( "Subtract " + Number( b ) + "from both sides", Equality( Number( a - b ), c ) )
			//case (Number( a ), Sum( Number( b ), c ) ) if ( a < b ) => rv = rv :+ Rule( "Subtract " + Number( a ) + "from both sides", Equality( Number( 0 ), Sum( Number( b - a ), c ) ) )
			case (Sum( a, b ), Sum( c, d ) ) if ( a == c ) => rv = rv :+ Rule( "Removal of common term " + a, Equality( b, d ) )
			//case (Sum( Number( a ), b:Expr ), Sum( Number( c ), d:Expr ) ) if( a > c ) => rv = rv :+ Rule( "Subtract " + Number( c ) + " from both sides", Equality( Sum( Number( a - c ), b ), d ) ) 
			//case (Sum( Number( a ), b:Expr ), Sum( Number( c ), d:Expr ) ) if( a < c ) => rv = rv :+ Rule( "Subtract " + Number( a ) + " from both sides", Equality( b, Sum( Number( c - a ), d ) ) ) 
			//case (Sum( Number( a ), Number( b ) ), Sum( Number( c ), Number( d ) ) ) => rv = rv :+ Rule( "Executing sum ", Equality( Number( a + b ), Number( c + d ) ) )
			case _ => Nil
		}
		//rv = rv ++ onesider( left, right )
		//rv = rv ++ onesider( right, left ).map( rule => rule.flipLeftAndRight )
		//rv = rv :+ Rule( "Switch left and right", Equality( right, left ) )
		rv
	}

}

/*
class InEquality extends Relation

// left > right
class GreaterThan extends Relation

class GreaterOrEqualThan extends Relation
*/
