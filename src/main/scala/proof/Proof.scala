package galileo.proof

import galileo.environment.Environment
import galileo.expr.{Expr,Statement}

object Proof extends Enumeration{
	type Status = Value
	val Proven, Disproved, Unproven = Value
	
	def apply( left:Expr, relation:String, right:Expr ):Expr = relation match {
		case "=" => Proof( Equality( left, right ), List(), Proof.Unproven )
		//case "!=" => ProofInequality( left, right )
		//case ">" => ProofGreatherThan( left, right )
		//case ">=" => ProofGreatherThanOrEqual( left, right)
	}
	/*
	val rules:List[Rule] = {
		var rv:List[Rule] = List()
		//rv = rv :+ Numnber.rules()
		rv
	}
	*/
	//val maxDepth = 10
	// returns steps needed to get either
	// a truth, a falsitude or if no steps are returned the statement is 'unproven/uncertain'

	def prove(relation:Relation,depth:Int=10):Option[Proof] = {
		for( i <- 0 to depth )
		{
			proveInner(relation,depth) match {
				case Some( p ) => return Some( p )
				case _ => None
			}
		}
		
		return None
	}

	def proveInner(relation:Relation, depth:Int=10):Option[Proof] = {
		/*
		if( depth == 0 )
			return None //Proof(relation,List(), "unproven")

		def checker(left:Expr,right:Expr):Option[Rule] = (left,right) match {
			//case Number(a), Number(b) if ( a == b ) => rv = rv :+ Rule( "Equality of numbers", Truth( a + "=" + b ) )
			case (a:Expr,b:Expr) if ( a == b ) => Rule( "Equality of expressions", Truth( a + "=" + b ) )
			case (Number(a), Number(b)) => Rule( "Inequality of numbers", Falsitude( a + "!=" + b ) )
			case (Variable(a), Variable( b )) => Rule( "Inequality of variables", Falsitude( a + "!=" + b ) )
			//case (Sum( a, b ), c:Expr) if ( a == c ) => Rule( "Removal of common term " + a, Equality( b, Number( 0 ) ) )
			//case (Number( a ), Sum( Number( b ), c ) ) if ( b == a ) => rv = rv :+ Rule( "Removal of common term " + a, Equality( Number( 0 ), c ) )
			//case (Number( a ), Sum( Number( b ), c ) ) if ( a > b ) => rv = rv :+ Rule( "Subtract " + Number( b ) + "from both sides", Equality( Number( a - b ), c ) )
			//case (Number( a ), Sum( Number( b ), c ) ) if ( a < b ) => rv = rv :+ Rule( "Subtract " + Number( a ) + "from both sides", Equality( Number( 0 ), Sum( Number( b - a ), c ) ) )
			//case (Sum( a, b ), Sum( c, d ) ) if ( a == c ) => rv = rv :+ Rule( "Removal of common term " + a, Equality( b, d ) )
			//case (Sum( Number( a ), b:Expr ), Sum( Number( c ), d:Expr ) ) if( a > c ) => rv = rv :+ Rule( "Subtract " + Number( c ) + " from both sides", Equality( Sum( Number( a - c ), b ), d ) ) 
			//case (Sum( Number( a ), b:Expr ), Sum( Number( c ), d:Expr ) ) if( a < c ) => rv = rv :+ Rule( "Subtract " + Number( a ) + " from both sides", Equality( b, Sum( Number( c - a ), d ) ) ) 
			//case (Sum( Number( a ), Number( b ) ), Sum( Number( c ), Number( d ) ) ) => rv = rv :+ Rule( "Executing sum ", Equality( Number( a + b ), Number( c + d ) ) )
			case _ => None
		}

		val d = 1
		while( d < depth ){
			var ls = relation.left.conversions( d )
			var rs = relation.right.conversions( d )
			for( l <- ls ) {
				for( r <- rs ) {
					val attempt = checker( l, r )
					attempt.result match {
						case t:Truth => Some( Proof( relation, List( rule ), Proof.Proven ) )
						case f:Falsitude => Some( Proof( relation, List( rule ), Proof.Disproved ) )
						case _ => None
					}
				}
			}			
		}	
		*/
		

		//var steps:List[Rule] = List()
		//println( relation.rules( depth ) )
		//return None 
		for( rule <- relation.rules( depth ) ) {
			//println( "trying " + rule  + ", with result: " + rule.result )
			val attempt = rule.result
			attempt match {
				case t:Truth => return Some( Proof( relation, List( rule ), Proof.Proven ) )
				case f:Falsitude => return Some( Proof( relation, List( rule ), Proof.Disproved ) )
				case _ => None //e:Equality if ( e.left == e.right) => None // println( "I no know")
			}
			//val result = rule.result
		}
		/*
		// tail iteration here....
		if( depth < 10 )
		{
			// get new rules...
		}
		*/

		return None

		//return Some( Proof( relation, List(), Proof.Proven ) )
/*
		for( rule <- relation.rules( depth ) ) {
			val result = rule.result
			var rv:Option[Proof] = None
			result match {
				case t:Truth => rv = Some( Proof( relation, List( rule ), Proof.Proven ) )
				case f:Falsitude => rv = Some( Proof( relation, List( rule ), Proof.Disproved ) )
				case _ => None
			}
			if( rv.isDefined ) return rv
			// filter{ _.isDefined } map { _.get } // scala way of not returning None here

			println( "rv is notDefined")
			//println( "iterating...")
			val steps:List[Rule] = List( rule )
			var pp:Option[Proof] = None
			prove( result, depth + 1 ) match {
				case Some( p ) if( p.status != "unproven" ) => pp = Some( Proof( relation, steps ++ p.steps, p.status ) )
				case _ => None
			} 
			if( pp.isDefined ) return pp // Some( pp.get ) //Proof( relation, steps ++ pp.get.steps, pp.get.status ) )
			// now continue...
		}
		return None
		*/

		/*		case Truth => Proof( relation, steps :+ rule, "proven" ) // we're done here, it's been proven...
				case Falsitude => Proof( relation, steps :+ rule, "disproved" )
				case u => { 
					val p = prove( u, depth - 1 )
					if( // case _ if( prove( rule.result, depth - 1  ).status != "unproven" ) => Proof( relation, s)
			}
			*/
	}
}
 
// we prove relations, e.g. Prove( 4 == 5 - 1 ) or Prove( x = 2 * x + 5 - x - 5 )  or Prove( 4 > 3 )
case class Proof( relation:Relation, steps:List[Rule]=List(), status:Proof.Status ) extends Expr with Statement {
	// during this step we search for Truth... Or falsitude
	override def visit(env:Option[Environment]) = Proof.prove( relation, 0 ) match {
		case Some( p ) => p
		case None => this
	}
	override def toString():String = /* relation.rules.mkString( "\n") */
	status match {
		case Proof.Proven => "The statement " + relation + " is true, proven in the following steps\n" + steps.mkString( "\n")
		case Proof.Disproved => "The statement " + relation + " is false, shown in the following steps " + steps.mkString( "\n")
		case Proof.Unproven => "The veracity of statement " + relation + " can not be determined"
	}
	def info(env:Option[Environment]=None) = "Proof(" + relation + "," + steps + "," + status + ")" 
}

// Proof( 10 == 9 + 1, [], "unproven" )
// Proof( 10 == 10, [ "sum"], "proven" )
/*
case class Equality(left:Expr, right:Expr) extends Relation{
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
	def findProof( depth:Proof.maxDepth ):Option[Proof] = {
		// Let's say depth is 1...
		for( rule <- rules )
			rule.apply( this ) match {
				//case Truth => return 
				//case Falsitude => // counterproof
			}
	}
}
*/


// Proof as a verb
//case class Proof(left:Expr, relation:Relation, right:Expr) extends Expr{}
