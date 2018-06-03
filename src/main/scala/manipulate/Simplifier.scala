package galileo.manipulate

import galileo.constants.Constant
import galileo.expr._
import galileo.linalg.Matrix
import galileo.proof.Conversion
import galileo.tensor.Tensor
import galileo.trigonometry.{CosF1,SinF1,TrigF1}

import scala.collection.mutable.ListBuffer

trait SimplifierTrait {
	def simplify(expr:Expr,depth:Int=10,width:Int=5):Expr
}

class ClosedFormSimplifier extends SimplifierTrait {
	def simplify(expr:Expr,depth:Int=10,width:Int=5):Expr = expr.expand.simplify.visit()
}
object ComplexityMinimizingSimplifier {
	val complexityCache = collection.mutable.Map[Expr,Int]()
}
// Simplify by creating an expression with a low complexity
class ComplexityMinimizingSimplifier extends SimplifierTrait {
	// each expression has a complexity score
	// this simplifier attempts to simplify expressions by lowering that score
	def simplify(expr:Expr,depth:Int=10,width:Int=5):Expr = {
		if( depth == 0 )
			return expr

		val minC = complexity( expr )
		val minE = expr

		// optimize this further... exhaustive search for now
		//print( "At depth " + 10-depth + ", num conversions " + conversions.size + "\r" )
		val es = conversions(expr,depth).map( conversion => conversion.expr )
		if( es.isEmpty )
			return expr

		//println( "At depth " + depth + ", num conversions " + es.size + "\r" )
		//println( "depth" + depth )

		val cs = es.map( expr => complexity( expr ) )

		//println( "Considering conversions: " + es )
		// this finds up to number width better expr
		// todo: also include a few stragglers - don't always be greedy
		val best = ListBuffer( ( minC, expr ) )
		for( (c,e) <- cs.zip( es ) ) {
			if( best.size < width ) 
				best += ((c,e))
			else {
				val maxi = best.zipWithIndex.maxBy(_._1._1)._2
				val maxc = best( maxi )._1
				if( c < maxc )
					best.insert(maxi,(c,e))
			}
		}

		// for best (number width, search more)
		best.map( { case (lc,le) => simplify(le,depth-1,width) } )

		// return expression with lowest complexity 
		val maxi = best.zipWithIndex.minBy(_._1._1)._2
		best( maxi )._2
	}

	private def listComplexity(list:List[Expr],sort:(Expr,Expr)=>Boolean) = {
		var rv = list.map( elem => complexity( elem) ).sum
			// penalty for items 'out of order', penalty for size
			var penalty = list.size
			for( a <- 0 to { list.size - 2 } )
				if( !sort( list(a),list(a+1) ) )
					penalty=penalty+1
			rv + penalty
	}

	def complexity(expr:Expr):Int = {
		def complexityNoCache(e:Expr):Int = e match {
			case _:Number => 1
			case _:Constant => 2
			case _:Variable => 3
			case s:Sum => listComplexity(s.flatTerms, Sum.sort )
			case p:Product => listComplexity(p.flatFactors, Sum.sort ) * 2
			case Power(op,ex) => 4+complexity(op) + complexity(ex)
			case f:Fraction => 5+complexity(f.numerator)+complexity(f.denominator)
			case t:TrigF1 => 5 + complexity( t.e )
			case f:FunF1 => 6 + complexity( f.e ) 
			//case m:Matrix => 7 + m.components.map( component => complexity( component ) ).sum
			case t:Tensor => 8 + t.components.map( component => complexity( component ) ).sum
		}

		ComplexityMinimizingSimplifier.complexityCache.get( expr ) match {
			case Some(r) => r
			case None => 
				val r = complexityNoCache( expr )
				ComplexityMinimizingSimplifier.complexityCache(expr) = r
				r
		}
	}
	//private def listConversion(list:List[Expr]):

	def conversions( expr:Expr,depth:Int ):List[Conversion] = { 
		if (depth == 0)
			return List()
		
		expr match {
			case p:Product => {
				var rv:List[Conversion] = List()
				//convert each factor
				for( a <- { 0 to p.flatFactors.size-1 } ) {
					val factorConversions = conversions( p.flatFactors( a ), depth - 1)
					for( factorConversion <- factorConversions ) {
						val newFactors = p.flatFactors.updated(a,factorConversion.expr)
						rv = rv :+ Conversion( "Replace factor " + a + ": " + p.flatFactors( a ), Product( newFactors ).visit() )
					}
				}
				rv
			}
			case s:Sum => {
				var rv:List[Conversion] = List()
				//convert each term
				for( a <- { 0 to s.flatTerms.size-1 } ) {
					val termConversions = conversions( s.flatTerms( a ), depth - 1)
					for( termConversion <- termConversions ) {
						val newTerms = s.flatTerms.updated(a,termConversion.expr)
						rv = rv :+ Conversion( "Replace term " + a + ": " + s.flatTerms( a ), Sum( newTerms ).visit() )
					}
				}
				rv
			}
			case f:Fraction => //f.conversions( depth )
			case p:Power => {
				var rv:List[Conversion] = List() //List(Conversion( "this", p))
				(p.operand,p.exponent) match {
					case (SinF1(a),Number(2)) => rv = rv :+ Conversion( "sin^2->1-cos^2", Sum(Number(1),Product(Number(-1),Power(CosF1(a),Number(2)))) )
					case (CosF1(a),Number(2)) => rv = rv :+ Conversion( "cos^2->1-sin^2", Sum(Number(1),Product(Number(-1),Power(SinF1(a),Number(2)))) )
					case (a,b) => { 
						for( oc <- conversions( a, depth - 1 ); ec <- conversions( b, depth-1 ) )
							rv = rv :+ Conversion( "Convert operand", Power( oc.expr, ec.expr ).visit())
					}
				}
				rv
			}
			case t:Tensor => {
				var rv:List[Conversion] = List()
				for( a <- 0 to t.components.size-1 ) {
					val componentConversions = conversions( t.components( a ), depth - 1)
					for( componentConversion <- componentConversions ) {
						val newComponents= t.components.updated(a, componentConversion.expr)
						rv = rv :+ Conversion( "Replace component " + a + ": " + t.components( a ), Tensor( t.indices, newComponents ).visit() )
					}
				}
				rv
			}
			//case e:Expr => List( Conversion( "this", e ) )
			//case v:Variable => List(Conversion("this",v)
			//case n:Number => List()
			case _ => List() //{ println( "No nothing matched for " + expr.info() ); List() }
		}
	}
}