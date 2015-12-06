package galileo.expr

import galileo.complex.Complex
import galileo.environment.Environment
import galileo.linalg.Matrix
import galileo.proof.Conversion

// e1^e2
case class Power(operand:Expr, exponent:Expr) extends FunF2 {
	val a = operand
	val b = exponent
	override def toString() = {
		def factorToString( e : Expr) = e match {
			case Sum(_, _) ⇒ "(" + e.toString() + ")"
			case Product(_,_) => "(" + e.toString() + ")"
			case c:Complex => "(" + c.toString() + ")"
			case _ ⇒ e.toString()
		}
		factorToString( operand ) + "^" + factorToString(exponent)
	}
	override def toStringWithSign() =  ( operand, exponent ) match {
		case ( Number( v ), _ ) if v > 0  => "+" + this.toString() //" * " + e2.factorToString()
		//case ( Number( -1.0 ), _ ) => "-" + e2.factorToString() // support this?A?!?!?!?!
		//case ( Number( a ),_) => "+" => a.toString() //this.toString()
		case ( _,_) => "+" + this.toString()
	}
	
	override def visit(env:Option[Environment]=None):Expr = ( operand.visit( env ), exponent.visit( env ) ) match {
		case ( a, Number( 1 ) ) => a
		case ( a, Number( 0 ) ) => Number( 1 )
		// Don't just move from fraction to (negative powers), but we can move this:
		// a^(-n) -> 1/a^n
		case ( a, Number( n ) ) if ( n < 0 ) => Fraction( Number( 1 ), Power( a, Number( -n ) ) )
		case ( Number( l ), Number( r ) ) => Number( math.pow(l, r ) )
		case ( Power( o, el ), er ) => Power( o, Product( el, er ) ).visit()
		case ( Complex( r:Number, i:Number ), n:Number ) => ( Complex( r, i ) ^ n ).visit() // env )
		case ( p:Product,e) => Product( p.factors.map( f => Power( f, e ) ).toList ).visit()
		//case (c:Complex(Number(l),Number(r)), Number( e ) ) => c^r
		case ( m:Matrix, Number( n ) ) if ( n == 2 ) => Product( m, m ).visit()
		case ( m:Matrix, Number( n ) ) if ( n % 1 == 0 && n > 2 ) => Product( Product( m, m ), Power( m, Number( n - 2 ) ) ).visit()

		case ( l, r ) => Power( l, r )
  	}

  	override def info(env:Option[Environment]=None) = "Power(" + operand.info(env) + "," + exponent.info(env) + ")"

  	def conversions(depth:Int):List[Conversion] = {
		var rv:List[Conversion] = List()
		rv = rv :+ Conversion( "Normalize", this.visit() )
		return rv
	}
  	/*
  	override def eval(env:Option[Environment]=None):Expr = ( operand.eval( env ), exponent.eval( env ) ) match {
  		case ( Number( l ), Number( r ) ) => Number( math.pow(l, r ) )
		case ( Complex( r:Number, i:Number ), n:Number ) => ( Complex( r, i ) ^ n ).eval() // env )
		//case (c:Complex(Number(l),Number(r)), Number( e ) ) => c^r
		case ( l, r ) => Power( l, r )
  	} 
  	*/

  	// (a+b)^N -> (a+b)*(a+b) ... (a+b)
  	override def expand = (operand,exponent) match {
  		case (_:Sum,Number( n ) ) if ( n%1 == 0 && n > 0 ) => Product( List.fill(n.toInt)(operand) ).expand
  		case (_:Product,Number( n ) ) if ( n%1 == 0 && n > 0 ) => Product( List.fill(n.toInt)(operand) ).expand
  		case _ => this
  	}

  	// a^n.extractFactor(a) -> a^(n-1)
  	// a^n.extractFactor(a^m) -> a^(n-m)
  	override def extractFactor(possibleFactor:Expr):Option[Expr] = possibleFactor match {
  		case this.operand => Some( Power( operand, Diff( exponent, Number( 1 ) ) ).visit() )
  		//case this => Some( Number( 1 ) )
  		case Power(op,ex) if ( op == operand ) => Some( Power( operand, Diff( exponent, ex ) ).visit() ) 
  		case _ => None
  	} 

  	override def eval = (operand.eval,exponent.eval) match {
  		case (Number(a),Number(b)) => Number( math.pow(a,b) )
  		case (a:Expr,b:Expr) => Power(a,b)
  	}

  	// a^2, possible factors are a^2 and a
  	override def possibleFactors:List[Expr] = exponent match {
  		case Number( n ) if ( n < 0 ) => List()
  		case Number( 0 ) => List()
  		case _ => List( this ) ++ operand.possibleFactors
  	}

  	// We only look at operand for this, we ignore exponent
  	override def leadingVariable:Option[String] = operand.leadingVariable
}

object Square{
	def apply(e:Expr) = Power( e, Number( 2 ) )
}

object Cube{
	def apply(e:Expr) = Power( e, Number( 3 ) )
}
