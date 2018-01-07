package galileo.expr

import galileo.environment.Environment
import galileo.expr._
import galileo.trigonometry._
import galileo.linalg.Matrix
import galileo.tensor.Tensor

// dy / dx
// complete derivative
case class Derivative( y:Expr, x:Expr ) extends Expr {
	override def toString() = "deriv(" + y.toString() + "," + x.toString() + ")"
	def info(env:Option[Environment]=None) = this.toString()

	// This is interesting,
	// variables is only used for the chain rule -- not sure this function will ever be used
	def variables:List[Variable] = List() // To Be Determined
	// should we remove x from env before visiting?
	override def visit(env:Option[Environment]) = ( y.visit( env ), x.visit( env ) ) match{
		case (_,s:Statement) => ErrorExpr( "Can't derive with respect to " + s )
		case (_,m:Matrix) => ErrorExpr( "Can't derive with respect to " + m )
		case (_,t:Tensor) => ErrorExpr( "Can't derive with respect to " + t )
		case (Number(_),_) => Number( 0 )
		case (a:Variable, b:Variable ) if ( a == b ) => Number( 1 )
		case (a:Variable, b:Variable ) => Number( 0 ) // not a dependency
		case (s:Sum,b) => Sum( s.terms.map( term => Derivative( term, b ) ):_* ).visit()
		case (Product( e1, e2 ), b ) => Sum( 
			Product( e1, Derivative( e2, b ) ), 
			Product( Derivative( e1, b ), e2 )
		).visit()
		case (p:Product, b ) => {
			var terms:List[Expr] = Nil
			var leftFactors:List[Expr] = Nil
			for( i <- 0 until p.factors.size ) {
				val factor:Expr = p.factors( i )
				val rightFactors:List[Expr] = p.factors.takeRight( p.factors.size - i - 1 ).to[List]
				val term = Product( leftFactors ++ rightFactors :+ Derivative( factor, b ) ).visit()
				terms = terms :+ term
				leftFactors = leftFactors :+ factor  
			}
			Sum( terms ).visit()
		}
		// D(g/h) = ( D(g) h - g D( h ) ) / h^2
		case( Fraction(g,h),b:Variable) => h match {
			// common special case D( g/x^2)
			case Power(a,Number(2)) if( a == b ) => Fraction( Diff( Product( Derivative( g, b ), a ), Product( Number( 2 ), g ) ), Cube( a ) ).visit()
			case _ => Fraction( Diff( Product( Derivative( g, b ), h ), Product( g, Derivative( h, b ) ) ), Square( h ) ).visit()
		}

		case (ExpF1( e ), b ) => Product( ExpF1( e ), Derivative( e, b ) ).visit()
		case (LogF1( e ), b ) => Product( Power( e, Number( -1 ) ), Derivative( e, b ) ).visit()
		case (CosF1( e ), b ) => Product( Product( Number( -1 ), SinF1( e  ) ), Derivative( e, b ) ).visit()
		case (SinF1( e ), b ) => Product( CosF1( e  ), Derivative( e, b ) ).visit()
		case (TanF1( e ), b ) => Fraction( Product( Number( -1 ), Derivative( e, b ) ), Power( CosF1( e ), Number( 2 ) ) ).visit()
		// d f^g = f^g dg + g * f ^ ( g - 1 ) * df
		case (Power( o, e ), b ) => Sum(
			Product( e, Power( o, Sum( e, Number( -1 ) ) ), Derivative( o, b ) ),
			Product( Derivative( e, b ), Power( o, e ) )
		).visit()
		// Chain Rule
		/*
		case (_,e:Expr) => {
			e.variables()
		}
		*/
		/*
		case (Variable( name ), v: => if( name == v.name ) Number( 1 ) else Number( 0 )
		case Sum( e1, e2 ) => Sum( e1 derive v, e2 derive v )
		case Product( e1, e2 ) => Sum( Product( e1, e2 derive v), Product( e1 derive v, e2 ) )
		case Power( e1, e2 ) => Product( e2, Power( e1, Sum( e2, Number( -1 ) ) ) )
		case ExpF1( e ) => Product( ExpF1( e ), e derive v )
		case LogF1( e ) => Product( Power( e, Number( -1 ) ), e derive v )
		case CosF1( e ) => Product( Product( Number( -1 ), SinF1( e  ) ), e derive v )
		case SinF1( e ) => Product( CosF1( e  ), e derive v )
		*/
	} 
}
