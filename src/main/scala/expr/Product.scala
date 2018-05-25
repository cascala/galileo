package galileo.expr

import galileo.complex._
import galileo.constants._
import galileo.environment.Environment
import galileo.manipulate.Simplify
import galileo.linalg._
import galileo.trigonometry._
import galileo.proof.Conversion

object Product {
	def apply( factors:List[Expr] ):Product = Product( factors:_* )

	val sort:(Expr,Expr)=>Boolean = { (a:Expr, b:Expr) => (a,b) match {
    case ( m:Matrix, _ ) => false
    case ( _, m:Matrix ) => false
		case ( c:Complex, d:Complex ) => false
		case ( c:Constant, d:Constant ) => c.shortName < d.shortName
		case ( c:Constant, d:Complex ) => false
		case ( c:Constant, n:Number ) => false // pref order 5*pi
		case ( c:Constant, d:Expr ) => true // pi * (BLAH)
		//case ( c:ConstantE, d:ConstantPi ) => true // handled by comparing shortname
		//case ( c:ConstantPi, d:ConstantE ) => false
		case ( c:Complex, d:Number) => false // numbers go before complex numbers
		case ( c:Complex, d:Expr ) => true
		case ( Number( x ), Number( y ) ) => x < y
		case ( Number( x ), Variable( y ) ) => true // numbers before variables
		case ( Number( x ), _:Expr ) => true
		case ( Variable( x ), Number( y ) ) => false
		case ( Variable( x ), Variable( y ) ) => x < y
		case ( Variable( x ), Power( Variable( y ), z:Expr ) ) => x < y
    case ( x:Variable, SinF1(y) ) if( y ==x ) => true // darn, somehow TrigF1 is not accepted
    case ( x:Variable, CosF1(y) ) if( y ==x ) => true
    
    // for the same variable
    // cos < asin < atan < cos < sin < tan < exp < log 
    // cos < sin < tan, simple alphabetical order
    case ( x:TrigF1, y:ExpF1 ) if ( x.e == y.e ) => true
    case ( x:TrigF1, y:LogF1 ) if ( x.e == y.e ) => true
    case ( x:AtrigF1, y:ExpF1 ) if ( x.e == y.e ) => true
    case ( x:AtrigF1, y:LogF1 ) if ( x.e == y.e ) => true
    case ( x:AtrigF1, y:TrigF1 ) if ( x.e == y.e ) => true

    case ( CosF1( x ), SinF1( y ) ) if ( x == y ) => true
    case ( CosF1( x ), TanF1( y ) ) if ( x == y ) => true
    case ( SinF1( x ), TanF1( y ) ) if ( x == y ) => true
    case ( SinF1( x ), CosF1( y ) ) if ( x == y ) => false
    case ( TanF1( x ), SinF1( y ) ) if ( x == y ) => false
    case ( TanF1( x ), CosF1( y ) ) if ( x == y ) => false

    // ..
    case (a:Variable,b:FunF1) if( a != b.e ) => Product.sort(a,b.e)
    case (a:FunF1,b:Variable) if( a.e != b) => Product.sort(a.e,b)
    case (a:FunF1,b:FunF1) if( a.e != b.e ) => Product.sort(a.e,b.e)


    // powers before for trig functions
    case (_:Power,_:FunF1) => true
    case (_:FunF1,_:Power) => false
    case (Power(_:Variable,_),Power(_:FunF1,_)) => true
    case (Power(_:FunF1,_),Power(_:Variable,_)) => false
    
    // for FunF1 of different variables, look at underlier
    case (f:FunF1,g:FunF1) => sort(f.e,g.e)
    case ( Power(a,b),Power(c,d)) => sort(a,c)
    case (a:Expr,Power(b,c)) => sort(a,b)
    case (Power(a,b),c:Expr) => sort(a,c)
		case (_,_) => (a.leadingVariable,b.leadingVariable) match {
      case (Some(s),Some(t)) => s < t 
      case (Some(_),None) => false // variables after constants 
      case (None,Some(_)) => true  // variables after constants
      case _ => false
    }
  } }
  val neutralElement:Number = Number( 1 )
}

case class Product( factors:Expr*) extends Expr with FunMany {
	override def toString():String = factors.map( factor => factor.factorToString() ).mkString( "*" ) // be careful -- there is a lower * as well...
	def info(env:Option[Environment]=None) = "Product(" + factors.map( factor => factor.info(env) ).mkString(",") + ")"
  val elements = factors.toList
  override def denominatorToString():String = "(" + toString() + ")"
  override def toStringWithSign() = factors(0) match {
		case Number( v ) if v > 0  => "+" + this.toString() //" * " + e2.factorToString()
    case Number( v ) if v < 0 => {
      val newFactors = factors.updated( 0, Number( -v ) )
      "-" + Product( newFactors:_* ).toString()
    } 
		case _ => "+" + this.toString()
	}

  override def possibleFactors:List[Expr] = this.flatFactors.map( factor => factor.possibleFactors ).flatten

    // non-nested product factors
    // this is like flatten
  	override lazy val flatFactors:List[Expr] = this.factors.map( factor => factor.flatFactors ).toList.flatten

  	override def eval() = Product( factors.map( factor => factor.eval() ):_* ).visit() // eval will replace all vars and constants, the rest is just turning it into numbers

    def conversions(depth:Int):List[Conversion] = {
      // always normalize
      var rv:List[Conversion] = List()
      rv = rv :+ Conversion( "Normalize", this.visit() )
      //rv = rv :+ 
      return rv
    }

    // This is structurally very similar to the simplification for Sum
  	override def visit( env:Option[Environment]=None):Expr = {
  		def pairProduct( a:Expr, b:Expr ):Option[Expr] = (a,b) match {
  			case (Number( 0 ), _ ) => Some( Number( 0 ) )
  			case (Number( 1 ), e ) => Some( e )
  			case (Number( a ), Number( b ) ) => Some( Number( a * b ) )
        case (_:Number,_:Variable) => None // make this explicit
        case (_:Number,_:Constant) => None 
        case (_:Number,_:TrigF1) => None
  			case ( a, Fraction( Number( 1 ), c ) ) => Some( Fraction( a, c ).visit() )
  			case ( a, Fraction( b, c ) ) if ( a == c ) => Some( b.visit() )
        //case ( a, Fraction( b, c ) ) if ( -a == c ) => (-b).visit()
			  case ( Fraction( a, b ), c:Expr ) if ( b == c ) => Some( a.visit() ) //Fraction( Product( a, b ), c ).visit()
  			case ( Fraction( a, b ), Fraction( c, d ) ) => Some( Fraction( Product( a, c ), Product( b, d ) ).visit() )
        
  			case ( Number( a ), Fraction( b, Number( c ) ) ) if ( a % c == 0 ) => pairProduct( Number( a / c ), b )//.visit()
  			case ( Fraction( a, Number( b ) ), Number( c ) ) if ( c % b == 0 ) => pairProduct( Number( c / b ), a )//.visit()
  			
        case ( a, Fraction( b, c ) ) => Some( Fraction( Product( a, b ), c ).visit() )
        case ( Fraction( a, b ), c ) => Some( Fraction( Product( a, c ), b ).visit() )
  			
  			case ( Power( a, b ), Power( c, d ) ) if ( a == c ) => Some( Power( a, Sum( b, d) ).visit() )
        case ( Power( a, b ), Power( c, d ) ) if ( a == c && b == -d) => Some( Number( 1 ) )
   			case ( a, Power( b, c ) ) if ( a == b ) => Some( Power( a, Sum( c, Number( 1 ) ) ).visit() )
        case ( _, Power( _, _ ) ) =>  None // make this explicit
  			case ( Power( a, b ), c ) if ( a == c ) => Some( Power( a, Sum( b, Number( 1 ) ) ).visit() )
  			
        // Matrix multiplications don't simplify (no A^2 needed)
  			case (l:Matrix,r:Matrix) => Some( ( l * r.toDenseMatrix ).visit() )
        case (l:Matrix,e) => Some( ( l * e ).visit() )
        case (l, r:Matrix ) => Some( ( r * l ).visit() )
     
        // WE SHOULD NOT DO THIS WHEN A IS A SUM?!
        case (a,b) if (a == b ) => Some( Power( a, Number( 2 ) ).visit() ) // this goes against 'expand'
        case (_:Variable,_:Variable) => None //Some( Product( a, b ) )// a!=b, make this explicit
        // Some more elaborate cases
        case ( Sum(a,Fraction(b,c)), d ) if ( c == d ) => Some( Sum( Product( a, c ), b ).visit() ) // product is simpler than fraction
  			case ( Sum(Fraction(a,b),c), d ) if ( b == d ) => Some( Sum( a, Product( b, c ) ).visit() )
        
        case (c:Product,d:Product) => Some( Product( c.flatFactors ++ d.flatFactors ).visit() )
        case _ => None
          //println( "product.visit unhandled:a:" + a.info() + ",\nb:" + b.info() )
          //Product( a, b )
        //}
      }
    
      val fs = Product( this.factors.map( f => f.visit( env ) ):_* ).flatFactors.sortWith( Product.sort )
      expressify( scan( Product.neutralElement, fs, pairProduct ) ) //(a,b) => Some( pairProduct(a,b ) ) ) )
  }

  // extract a factor
  // e.g. if this == Product( a, b, c ), extractFactor( b ) will return Product( a, c )
  override def extractFactor(possibleFactor:Expr):Option[Expr] = {
    if( possibleFactor == this ) return Some( Number( 1 ) )

    // chekc if b exists in Product(a,b,c)
    val found = this.factors.map( factor => factor.extractFactor( possibleFactor ) ).zipWithIndex.find( { case (e:Option[Expr],i:Int) => e match { 
      case Some(_) => true 
      case None => false
    } } )

    found match { 
      // Found one - splice factors and re-insert
      case Some((Some(e),i:Int)) => Some( expressify( factors.toList.updated( i, e ) ) )
      case Some(_) => None
      case None => None
    }
  }

  // Filter out Number( 1 ), then turn a list of factors into an expr, likely a Product
  def expressify( l:List[Expr]):Expr = l.filter( factor => factor != Number( 1 ) ) match {
    case Nil => Number( 1 )
    case a :: Nil => a 
    case b => Product( b )
  }

  // a * ( c + d ) -> a * c + d * a
  // Product turns into sum, potentially
  override def expand = {
    // find all factors thar are a sum or are products containing a sum
    val sumIndex = flatFactors.zipWithIndex.find( //{ case (f,i) => sumFinder( f ) } )
      { case (f:Expr,i:Int) => f match { 
      case s:Sum => true 
      case _ => false
    } } )

    sumIndex match {
      // This replaces the product with a sum
      // a * (b +c ) * d -> a * b * d + a * c * d
      case Some((s:Sum,i:Int)) => { 
        Sum( s.terms.toList.map( term => Product( flatFactors.toList.updated(i,term ) ) ) ).expand.visit() // do we need visit here?
      }
      case _ => this
    }
  }

  
  override def factor:Expr = {
      def factorPair(a:Expr,b:Expr):Option[Expr] = None
      //    Some( Product(a,b))
      //}
      // factorize all factors first
      val factors = Product( this.flatFactors.map( factor => factor.factor ):_* ).flatFactors.sortWith( Product.sort )
      expressify( scan( Product.neutralElement, factors, factorPair ) )
    } 

  override def simplify:Expr = Product( this.flatFactors.map( factor => Simplify( factor ) ).toList ).visit()
}
