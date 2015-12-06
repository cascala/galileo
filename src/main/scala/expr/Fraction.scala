package galileo.expr

import galileo.environment.Environment
import galileo.linalg.Matrix
import galileo.proof.Conversion

case class Fraction(numerator:Expr, denominator:Expr) extends FunF2 {
	val a = numerator
	val b = denominator 
	override def toString() = numerator.factorToString() + "/" + denominator.denominatorToString()
	override def denominatorToString() = factorToString()
	override def factorToString() = "(" + toString() + ")"
	override def toStringWithSign():String = "+(" + toString() + ")"
	override def info(env:Option[Environment]=None) = "Fraction(" + numerator + "," + denominator + ")" 

	def conversions(depth:Int):List[Conversion] = {
		var rv:List[Conversion] = List()
		rv = rv :+ Conversion( "Denominator move to numerator", Product( this.numerator, Power( this.denominator, Number( -1 ) ) ) )
		rv = rv :+ Conversion( "Normalize", this.visit() )
		this.numerator match {
			case f:Fraction => { 
				val p = Product( f.denominator, this.denominator )
				rv = rv :+ Conversion( "Fraction denominator is moved", Fraction( f.numerator, p ) )
				
				//for( c <- p.conversions( depth - 1 ) )
				//	rv = rv :+ Conversion( )

			}
			case Number( 1 ) => rv = rv :+ Conversion( "1/x->x^(-1)", Power( denominator, Number( -1 ) ) )
			//case Power(o,e) => rv = rv :+ Conversion( "Denominator move to n")
			case _ => None
		}
		return rv
	}

	// Todo:
	// * Before visiting num and den, look for common factors and eliminate them
	override def visit( env:Option[Environment] = None ):Expr = ( numerator.visit( env ), denominator.visit( env ) ) match {
		case (Number(0),Number(0)) => Fraction( Number(0), Number(0)) // todo, apply limits, l'hopital?
		case (a:Expr,b:Expr) if ( a == b ) => Number( 1 )
		case (a, Number( 0 )) => Fraction( a, Number( 0 ) )
		case (Number(0),_) => Number( 0 )
		case (m:Matrix, e ) => ( m * Fraction( Number( 1 ), e ) ).visit()
		
		// some common, relatively short cases that we can explicity map
		// The simplify method finds common factors in more complex fractions already
		case (Product(a,b),c) if ( a == c ) => b
		case (Product(a,b),c) if ( -a == c ) => (-b)
		case (Product(a,b),c) if ( b == c ) => a
		case (a:Expr,Product(b,c)) if ( a == b ) => Fraction( Number( 1 ), c ).visit( env )
		case (a:Expr,Product(b,c)) if ( a == c ) => Fraction( Number( 1 ), b ).visit( env )
		case (a:Expr,Product(b,c)) if ( -a == b ) => Fraction( Number( -1 ), c ).visit( env )
		case (a:Expr,Product(b,c)) if ( -a == c ) => Fraction( Number( -1 ), b ).visit( env )

		//case (Product(a,b),c) if ( a == c ) => b
		


		case (Product(a,b),Product(c,d)) if (a == c ) => Fraction( b, d ).visit( env )
		case (Product(a,b),Product(c,d)) if (a == d ) => Fraction( b, c ).visit( env )
		case (Product(a,b),Product(c,d)) if (b == c ) => Fraction( a, d ).visit( env )
		case (Product(a,b),Product(c,d)) if (b == d ) => Fraction( a, c ).visit( env )

		case (Product(a,b),Product(c,d)) if (-a == c ) => Fraction( -b, d ).visit( env )
		case (Product(a,b),Product(c,d)) if (-a == d ) => Fraction( -b, c ).visit( env )
		case (Product(a,b),Product(c,d)) if (-b == c ) => Fraction( -a, d ).visit( env )
		case (Product(a,b),Product(c,d)) if (-b == d ) => Fraction( -a, c ).visit( env )	

		// Don't move from denominator to numerator just because
		// But we can move this: a/b^(-n) -> a*b^n
		case ( a:Expr, Power( b:Expr, Number( c ) ) ) if ( c < 0 ) => Product( a, Power( b, Number( -c ) ) ).visit( env )
	
		case ( Fraction( a, b ), Fraction( c, d ) ) if ( b == d ) => Fraction( a, c ).visit() //Product( a, d ), Product( b, c ) ).visit()
		case ( Fraction( a, b ), Fraction( c, d ) ) if ( a == c ) => Fraction( d, b ).visit() //Product( a, d ), Product( b, c ) ).visit()
		case ( Fraction( a, Number( b ) ), Fraction( c, Number( d ) ) ) if ( d % b == 0 ) => Fraction( Product( Number( d / b ), a ), c ).visit()
		case ( Fraction( Number( a ), b ), Fraction( Number( c ), d ) ) if ( a % c == 0 ) => Fraction( Product( Number( a / c ), d ), b ).visit()
		case ( Fraction( a, b ), Fraction( c, d ) ) => Fraction( Product( a, d ), Product( b, c ) ).visit()
		case ( Fraction( a, b), c:Expr ) => Fraction( a, Product( b, c ) ).visit()
		case ( a, Fraction( b, c ) ) => Fraction( Product( a, c ), b ).visit()
		
		//case (Number( a ), Number( b )) if ( a == b ) => Number( 1 ) //Number( a / b ) 
		//case (Number( a ), Number( b )) => Number( a / b ) 
		case ( a, Number( 1 ) ) => a
		//case ( Number( 1 ), a ) => Fraction( Number( 1 ), a )
		case ( a:Expr, b:Expr ) if ( a == b ) => Number( 1 )
		case ( Number( a ), Number( b ) ) if ( a > 0 && b < 0) => Fraction( Number( -a ), Number( -b ) ).visit()
		case ( Number( a ), Number( b ) ) if ( a < 0 && b < 0) => Fraction( Number( -a ), Number( -b ) ).visit()
		case ( Number( a ), Number( b ) ) if ( a % b == 0 ) => Number( a / b )
		case ( Number( a ), Number( b ) ) if ( b % a == 0 && b*a>0) => Fraction( Number(  1 ), Number(  b / a ) )
		case ( Number( a ), Number( b ) ) if ( b % a == 0 && b*a<0) => Fraction( Number( -1 ), Number( -b / a ) )

		// this seems to be too agressive	
		// case ( Number( 1 ), Sum( Fraction( a, b ), c ) ) => Fraction( b, Sum( a, Product( b,c ) ) ).visit()

		//case ( a, Sum( b, Fraction( c, d ) ) ) => Fraction( Product( a, d ), Sum( Product( b, d ), c ) ).visit()
		//case ( a, Sum( Fraction( b, c ), d ) ) => Fraction( Product( a, c ), Sum( b, Product( c, d ) ) ).visit()

		//case ( Product( Number( a ), b ), s:Sum ) => Number( a )
		//case (n:Product,d:Product) if( Product.commonExpressions( n , d ) ) => n.
		case (a:Expr, b:Expr) => Fraction( a, b )
		
	}

	override def eval():Expr = ( numerator.eval(), denominator.eval() ) match {
		//case ( Number( a ), Number)
		case ( Number( a ), Number( b ) ) => Number( a / b )
		case ( a:Expr, b:Expr ) => Fraction( a, b )
	}

	// simplify by finding common factors in the numerator and denominator of a fraction
	override def simplify:Expr = {
		// simplify things with simple num/denom, like a^2/a^3
		def simplifyOne(f:Fraction):Option[Fraction] = (f.numerator,f.denominator) match {
			case (a:Expr,b:Expr) if ( a == b ) => Some( Fraction( Number( 1 ), Number( 1 ) ) )
			case (Number(a),Number(b)) if ( a % b == 0 ) => Some( Fraction( Number( a / b ), Number( 1 ) ) )
			case (Number(a),Number(b)) if ( b % a == 0 ) => Some( Fraction( Number( 1 ), Number( b / a ) ) )
			case (Number(a),Number(b)) if ( -a == b ) => Some( Fraction( Number( -1 ), Number( 1 ) ) )
			case (a:Expr,Power(b,c)) if ( a == b && c != Number( 1 ) ) => Some( Fraction( Number( 1 ), Power( b, Sum( c, Number( -1 ) ).visit() ) ) )
			case (Power(a,b),c:Expr) if ( a == c && b != Number( 1 ) ) => Some( Fraction( Power( a, Sum( b, Number( -1 ) ).visit() ), Number( 1 ) ) )
			case (Power(a,b:Number),Power(c,d:Number)) if ( a == c && d.value > b.value ) => Some( Fraction( Number( 1 ), Power( a, Diff( d, b ) ) ) )
			case (Power(a,b),Power(c,d)) if ( a == c ) => Some( Fraction( Power( a, Diff( b, d ) ), Number( 1 ) ) )
			case (s:Sum,d:Expr) => s.extractFactor( d ) match {
				case Some(r) => Some( Fraction( r, Number( 1 ) ) ) // s = r * d, so simplify r * d / d to r
 				case None => None
			}
			case (p:Product,d:Expr) => p.extractFactor( d ) match {
				case Some(r) => Some( Fraction( r, Number( 1 ) ) ) // p = r * d, so simplify r * d / d to r
 				case None => None
			}
			case _ => None
		}

		val n = numerator.simplify.flatFactors
		val d = denominator.simplify.flatFactors

		var newn:List[Expr] = n
		var newd:List[Expr] = d
		var indn = 0 // numerator index
		var indd = 0 // denominator index

		// loop over all factors in numerator and denominator
		while( indn < newn.size && indd < newd.size ) {
			val candn = newn(indn)
			val candd = newd(indd)
			//println( "candn:" + candn.info() )
			//println( "candd:" + candd.info() )
			//println( "Calling simplyOne on:" + candn + "/" + candd )
			simplifyOne( Fraction( candn,candd ) ) match {
				case Some(newf) => { 
					//println( "Fraction" + Fraction(candn,candd) + " simplified to " + newf )
					newn = newn.updated( indn, newf.numerator )
					newd = newd.updated( indd, newf.denominator )
					indd = indd + 1
				}
				case None => indd = indd+1
			}
			
			// loop over all in indd first, then indn again
			if( indd >= newd.size ) {
				indn = indn+1
				indd = 0
			}
		}
		// then do a final sweep to clean up all remaining factor 'Number(1)' etc.
		Fraction(Product(newn),Product(newd)).visit()
	}

	override def expand = Fraction( numerator.expand, denominator.expand )

	override def extractFactor(possibleFactor:Expr):Option[Expr] = possibleFactor match {
		case Fraction(n,d) => (numerator.extractFactor(n),denominator.extractFactor(d)) match {
			case (Some(nn),Some(nd)) => Some( Fraction(nn,nd) )
			case _ => None
		}
		case e:Expr => (numerator.extractFactor( e ) ) match {
			case Some(nn) => Some(Fraction(nn,denominator))
			case _ => None
		}
	}
}
