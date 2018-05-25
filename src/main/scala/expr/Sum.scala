package galileo.expr

import galileo.complex._
import galileo.constants._
import galileo.environment.Environment
//import galileo.expr.FunF1
import galileo.linalg._
import galileo.manipulate.Simplify
import galileo.proof.Conversion
import galileo.trigonometry.{CosF1, SinF1, TrigF1 }

object Diff {
  def apply(a: Expr, b: Expr) = Sum(a, Product(Number(-1), b))
}

object Sum {
  val sort:(Expr,Expr)=>Boolean = (a: Expr, b: Expr) => (a, b) match {
    case (c: Complex, d: Complex) => false
    case (c: Constant, d: Constant) => c.shortName < d.shortName
    case (c: Constant, d: Complex) => false
    case (c: Constant, d: Expr) => true
    case (c: Complex, d: Number) => false // numbers go before complex numbers
    case (c: Complex, d: Expr) => true
    case (Number(x), Number(y)) => x < y
    case (Number(x), Variable(y)) => false // numbers after variables
    case (Number(x), _: Expr) => false // numbers after expressions
    case (_: Expr, _: Number) => true

    case (Product(_: Number, a: Expr), b:Expr ) => Sum.sort( a, b )
    case (a:Expr, Product(_: Number, b:Expr ) ) => Sum.sort( a, b )
    
    case (Product(_:Number, a: Expr, b:Expr), c:Expr ) => Sum.sort( Product( a, b ), c )
    case (a:Expr, Product(_:Number, b:Expr, c:Expr ) ) => Sum.sort( a, Product( b, c ) )

    case (Product(_:Number, a: Expr, b:Expr, c:Expr), d:Expr ) => Sum.sort( Product( a, b, c ), d )
    case (a:Expr, Product(_:Number, b:Expr, c:Expr, d:Expr ) ) => Sum.sort( a, Product( b, c, d ) )

    // generic product handler here
    // 3 x 3
    case (Product(a,b,c),Product(d,e,f)) => (a.leadingVariable,d.leadingVariable) match {
      case (Some(s),Some(t)) if (a==d) => Sum.sort(Product(b,c),Product(e,f))
      case (Some(s),Some(t)) if (s==t) => Sum.sort(a,d)
      case (Some(s),Some(t)) => s < t
      case _ => false
    }

    // 2 x 3
    case (Product(a,b),Product(c,d,e)) => (a.leadingVariable,c.leadingVariable) match {
      case (Some(s),Some(t)) if (a==c) => Sum.sort(b,Product(d,e))
      case (Some(s),Some(t)) if (s==t) => Sum.sort(a,c)
      case (Some(s),Some(t)) => s < t
      case _ => false
    }

    // 1 x 3
    case (a:Expr,Product(b,c,d)) => (a.leadingVariable,b.leadingVariable) match {
      //case (Some(s),Some(t)) if (a==b) => Sum.sort(b,Product(d,e))
      //case (Some(s),Some(t)) if (s==t) => Sum.sort(a,c)
      case (Some(s),Some(t)) => s < t
      case _ => false
    }

    // 3 x 2
    case (Product(a,b,c),Product(d,e)) => (a.leadingVariable,d.leadingVariable) match {
      case (Some(s),Some(t)) if (a==d) => Sum.sort(Product(b,c),e)
      case (Some(s),Some(t)) if (s==t) => Sum.sort(a,d)
      case (Some(s),Some(t)) => s < t
      case _ => false
    }

    // 2 x 2
    case (Product(a,b),Product(c,d)) => (a.leadingVariable,c.leadingVariable) match {
      case (Some(s),Some(t)) if (a==c) => Sum.sort(b,d)
      case (Some(s),Some(t)) if (s==t) => Sum.sort(a,c)
      case (Some(s),Some(t)) => s < t
      case _ => false
    }

    // 2 x 1
    case (Product(a,b),c) => (a.leadingVariable,c.leadingVariable) match {
      //case (Some(s),Some(t)) if (a==c) => Sum.sort(b,d)
      case (Some(s),Some(t)) if (s==t) => Sum.sort(a,c)
      case (Some(s),Some(t)) => s < t
      case _ => false
    }

    // 1 x 2
    case (a:Expr,Product(b,c)) => (a.leadingVariable,b.leadingVariable) match {
      //case (Some(s),Some(t)) if (a==b) => Sum.sort(b,Product(d,e))
      case (Some(s),Some(t)) if (s==t) => Sum.sort(a,b)
      case (Some(s),Some(t)) => s < t
      case _ => false
    }

    case ( Power(a,Number(c)),Power(b,Number(d)) ) if( a == b ) => c > d 
    case ( Power(a,_1),Power(b,_2)) if( a != b ) => sort( a, b )

    //case ( a:FunF1, b:FunF1 ) => sort( a.expr, b.expr ) 
    case ( CosF1( x ), SinF1( y ) ) if( x == y ) => true
    case ( SinF1( x ), CosF1( y ) ) if( x == y ) => false
    //case ( x:TrigF1, y:TrigF1 ) => sort( x, y ) // Don't do this ... creates cycle

    // Generic
    case (e:Expr, f:Expr) => ( e.leadingVariable, f.leadingVariable ) match {
      case (None,Some(s)) => true
      case (Some(s),None) => false
      case (Some(s),Some(t)) => s < t
      case _ => e < f // Expr ordering comparison, not numerical ordering
    }
  }

  val neutralElement: Number = Number(0)

  def apply(terms: List[Expr]): Sum = Sum(terms: _*)
}

case class Sum(terms: Expr*) extends FunMany {
  override lazy val flatTerms: List[Expr] = this.terms.map(term => term.flatTerms).toList.flatten
  val elements = terms.toList

  def info(env: Option[Environment] = None) = "Sum(" + terms.map(term => term.info(env)).mkString(",") + ")"

  override def factorToString() = "(" + toString() + ")"

  override def toString() = {
    //terms.map(term => term.termToString()).mkString("+")
    var rv = terms(0).toString()
    for( i <- 1 until terms.size )
      rv = rv + terms(i).toStringWithSign
    rv
  }

  override def eval() = Sum(terms.map(term => term.eval()): _*).visit() // eval will replace all vars and constants, the rest is just turning it into numbers

  def conversions(depth: Int): List[Conversion] = {
    // always normalize
    var rv: List[Conversion] = List()
    rv = rv :+ Conversion("Normalize", this.visit())
    return rv
  }

  /* simplification of sum: 
   # simplify all individual terms
   */
  override def simplify:Expr = Sum( flatTerms.map(term => Simplify(term).visit()).toList).visit() match {
    case s:Sum if ( s == this ) => s
    /*
    case s:Sum => s.flatTerms match {
      case ( Product( Number( -1 ), Power( CosF1( psi1:Expr ), Number( 2 ) ) ) :: Product( Number( 5 ), Power( SinF1( psi2:Expr ), Number( 2 ) ) ) :: Number( 1 ) :: Nil ) if ( psi1 == psi2 ) => Product( Number( 6 ), Power( SinF1( psi1:Expr ), Number( 2 ) ) )      
      case _ => s
    }
    */
    case e => e.simplify // OK to recurse
  }

  // Uses a generic scan function, used for both sums and products
  override def visit(env: Option[Environment] = None): Expr = {
    def pairSum(a: Expr, b: Expr): Option[Expr] = (a, b) match {
      case (Number(0), e) => Some(e)
      case (e, Number(0)) => Some(e)
      case (Number(a), Number(b)) => Some(Number(a + b))
      case (l: Matrix, r: Matrix) if (l.numRows == r.numRows && l.numCols == r.numCols) => Some((l.toDenseMatrix + r.toDenseMatrix).visit())
      case (l: Matrix, r: Matrix) => Some(ErrorExpr("Incompatible matrix dimensions for sum, (" + l.numRows + ", " + l.numCols + ") and (" + r.numRows + ", " + r.numCols + ")"))
      case (l: Matrix, r: Expr) => Some((l.toDenseMatrix + r).visit())
      case (l: Expr, r: Matrix) => Some((r.toDenseMatrix + l).visit())
      case (a, b) if (a == b) => Some(Product(Number(2), a).visit())
      case (a, Product(b, c)) if (a == c) => Some(Product(Sum(Number(1), b), c).visit())
      case (Product(a, b), Product(c, d)) if (b == d) => Some(Product(Sum(a, c), b).visit())
      // Basic trig.
      case (Number(1), Product(Number(-1), Power(CosF1(b), Number(2)))) => Some(Square(SinF1(b)))
      case (Product(Power(CosF1(b), Number(2)), Number(-1)), Number(1)) => Some(Square(SinF1(b)))
      case (Product(Number(-1), Power(CosF1(b), Number(2))), Number(1)) => Some(Square(SinF1(b)))
      case (Power(SinF1(a), Number(2)), Power(CosF1(b), Number(2))) if (a == b) => Some(Number(1))
      case (Power(CosF1(a), Number(2)), Power(SinF1(b), Number(2))) if (a == b) => Some(Number(1))
      case (Power(CosF1(a), Number(2)), Number(-1)) => Some(Product(Number(-1), Square(SinF1(a))))
      case (Power(SinF1(a), Number(2)), Number(-1)) => Some(Product(Number(-1), Square(CosF1(a))))

      // x * cos(a)^2 + y * sin(a)^2 -> ()
      /*
      case (Product(Number(x), Power(CosF1(a), Number(2))), Product(Number(y), Power(SinF1(b), Number(2)))) if (a == b && x <= -1 && y <= -1) =>
        Some(Sum(Product(Number(x + 1), Power(CosF1(a), Number(2))), Product(Number(y + 1), Power(SinF1(b), Number(2))), Number(-1)))
     
      case (Product(Number(x), Power(CosF1(a), Number(2)),b), Product(Number(y), Power(SinF1(c), Number(2)),d)) if (a == c && b == d && x <= -1 && y <= -1) =>
        Some(Sum(Product(Number(x + 1), Power(CosF1(a), Number(2)),b), Product(Number(y + 1), Power(SinF1(c), Number(2)),d), Product( d, Number(-1))) )

      case (Product(Number(x), Power(CosF1(a), Number(2))), Product(Number(y), Power(SinF1(b), Number(2)))) if (a == b && x >= 1 && y >= 1) =>
        Some(Sum(Product(Number(x - 1), Power(CosF1(a), Number(2))), Product(Number(y - 1), Power(SinF1(b), Number(2))), Number(1)))
      */

      // a * x + b * x -> (a+b)*x
      case ( Product( Number( a ), x ), Product( Number( b ), y ) ) if ( x == y ) => Some( Product( Number( a + b ), x ) )
      // a * x + x -> (a+1) * x
      case ( Product( Number( a ), x ), y ) if ( x == y ) => Some( Product( Number( a + 1 ), x ) )
      // x + a * x -> (a+1) * x
      case ( x, Product( Number( a ), y ) ) if ( x == y ) => Some( Product( Number( a + 1 ), x ) )

      //case (Power(SinF1(a),Number(2)),Product(Number(-1),Power(CosF1(b),Number(2))) if ( a == b ) => Some( Number( 1 ) )
      //case (Product( a, b), Fraction( c, d ) ) if ( a == c && b == Power( d, Number( -1 ) ) ) => Product( Number( 2 ), a, b )
      // Nice application of pattern matching to help with factorization
      // 7 * a * b + 3 * a * b -> 10 * a * b
      case (p1: Product, p2: Product) => (p1.factors.toList, p2.factors) match {
        // Need to also simplify things like a * sin(b)^2+ a * cos( b ) ^ 2
        //case ( a :: b :: c, d :: e :: f)
        case (Power(CosF1(a), Number(2)) :: b, Power(SinF1(c), Number(2)) :: d) if (a == c && b == d ) => Some( Product( b ) )

        case (a :: Power(CosF1(b), Number(2)) :: Power(SinF1(c), Number(2)) :: d, e :: Power(SinF1(f), Number(4)) :: g)
          if (a == e && b == c && b == f && d == g) => Some( Product(d :+ a :+ Power( SinF1(b),Number(2) ) ) )
        case (a :: Power(CosF1(b), Number(2)) :: c, d :: Power(SinF1(e), Number(2)) :: f) if (a == d && b == e && c == f) => Some( Product( c:+ a ) )
        case (a :: Power(SinF1(b), Number(2)) :: c, d :: Power(CosF1(e), Number(2)) :: f) if (a == d && b == e && c == f) => Some( Product( c:+ a ) )
        case (a :: Power(CosF1(b), Number(2)) :: Nil, d :: Power(SinF1(e), Number(2)) :: Nil) if (a == d && b == e) => Some(Product(a))
        case (a :: Power(SinF1(b), Number(2)) :: Nil, d :: Power(CosF1(e), Number(2)) :: Nil) if (a == d && b == e) => Some(Product(a))
        case (a :: b :: Power(CosF1(c), Number(2)) :: d, e :: f :: Power(SinF1(g), Number(2)) :: h) if (a == e && b == f && c == g && d == h) => Some(Product(d :+ a :+ b))
        case (a :: b :: Power(SinF1(c), Number(2)) :: d, e :: f :: Power(CosF1(g), Number(2)) :: h) if (a == e && b == f && c == g && d == h) => Some(Product(d :+ a :+ b))
        case (a :: b :: c :: Power(CosF1(d), Number(2)) :: e, f :: g :: h :: Power(SinF1(i), Number(2)) :: j) if (a == f && b == g && c == h && d == i && e == j) => Some(Product(e :+ a :+ b :+ c))
        case (a :: b :: c :: Power(SinF1(d), Number(2)) :: e, f :: g :: h :: Power(CosF1(i), Number(2)) :: j) if (a == f && b == g && c == h && d == i && e == j) => Some(Product(e :+ a :+ b :+ c))

        case (Number(m) :: a, Number(n) :: b) if (a == b) => Some( Product( a:+ Number( m + n ) ).visit() )
        case (Number(n) :: a, b) if (a == b) => Some( Product( a:+ Number( n + 1 ) ).visit() )
        case (a, Number(n) :: b) if (a == b) => Some( Product( a:+ Number( n + 1 ) ).visit() )


        case _ => None
      }
      case (Fraction(a: Expr, b: Expr), Fraction(c: Expr, d: Expr)) if (b == d) => Some(Fraction(Sum(a, c), b).visit())
      // a/(x*d) + c / d = ( a + c * x ) / ( x * d )
      case (Fraction(a: Expr, Number(b)), Fraction(c: Expr, Number(d))) if (b % d == 0) => Some(Fraction(Sum(Product(c, Number(b / d)), a), Number(b)).visit())
      case (Fraction(a: Expr, Number(b)), Fraction(c: Expr, Number(d))) if (d % b == 0) => Some(Fraction(Sum(Product(a, Number(d / b)), c), Number(d)).visit())
      case (Fraction(Number(a), Number(b)), Fraction(Number(c), Number(d))) => Some(Fraction(Number(a * d + b * c), Number(b * d)))
      case (Fraction(a, Number(b)), Number(c)) => Some(Fraction(Sum(a, Number(b * c)), Number(b)).visit())
      case (Number(a), Fraction(b, Number(c))) => Some(Fraction(Sum(b, Number(a * c)), Number(c)).visit())

      case (a, b) if (a == -b) => Some(Number(0))
      //case ( Fraction( a, b ), c ) => Fraction( Sum( a, Product( b, c ) ), b ).visit()
      //case ( a, Fraction( b, c ) ) => Fraction( Sum( b, Product( a, c ) ), c ).visit()
      //case (Fraction(a, b), Fraction(c, Product(d, e))) if (b == e) => Some(Product(Fraction(a, b), Sum(Number(1), Fraction(c, d))).visit())
      case (Number(1), Fraction(a, Sum(b, c))) if (a == -c) => Some(Fraction(b, Sum(b, c)).visit())
      case (Number(1), Fraction(a, Sum(b, c))) if (a == -b) => Some(Fraction(c, Sum(b, c)).visit())
      case (a, b) => None
    }

    val ts = Sum(this.terms.map(t => t.visit(env)): _*).flatTerms.sortWith(Sum.sort)
    ts match {  
      case ( start :: Product( Number( n ), a:Expr, b:Expr ) :: mid :: Product( c:Expr, d:Expr ) :: end ) if ( a == c && b == d ) => Sum( start :: Product( Number( n + 1 ), a, b ) :: mid :: end ).visit()
      case ( start :: Product( a:Expr, b:Expr ) :: mid :: Product( Number( m ), c:Expr, d:Expr ) :: end ) if ( a == c && b == d ) => Sum( start :: Product( Number( 1 + m ), a, b ) :: mid :: end ).visit()
      case ( start :: Product( Number( n ), a:Expr, b:Expr ) :: mid :: Product( Number( m ), c:Expr, d:Expr ) :: end ) if ( a == c && b == d ) => Sum( start :: Product( Number( n + m ), a, b ) :: mid :: end ).visit()
      case ( start :: Product( Number( n ), a:Expr, b:Expr, c:Expr ) :: mid :: Product( Number( m ), d:Expr, e:Expr, f:Expr ) :: end ) if ( a == d && b == e && c == f) => Sum( start :: Product( Number( n + m ), a, b, c ) :: mid :: end ).visit()
      case ( start :: Product( a:Expr, b:Expr ) :: mid :: Product( c:Expr, d:Expr ) :: end ) if ( a == c && b == d ) => Sum( start :: Product( Number( 2 ), a, b ) :: mid :: end ).visit()
      case ( start :: Product( a:Expr, b:Expr, c:Expr ) :: mid :: Product( d:Expr, e:Expr, f:Expr ) :: end ) if ( a == d && b == e && c == f) => Sum( start :: Product( Number( 2 ), a, b, c ) :: mid :: end ).visit()
  
      case ( Product( Number( n ), a:Expr, b:Expr ) :: mid :: Product( c:Expr, d:Expr ) :: end ) if ( a == c && b == d ) => Sum( Product( Number( n + 1 ), a, b ) :: mid :: end ).visit()
      case ( Product( a:Expr, b:Expr ) :: mid :: Product( Number( m ), c:Expr, d:Expr ) :: end ) if ( a == c && b == d ) => Sum( Product( Number( 1 + m ), a, b ) :: mid :: end ).visit()      
      case ( Product( Number( n ), a:Expr, b:Expr ) :: mid :: Product( Number( m ), c:Expr, d:Expr ) :: end ) if ( a == c && b == d ) => Sum( Product( Number( n + m ), a, b ) :: mid :: end ).visit()
      case ( Product( Number( n ), a:Expr, b:Expr, c:Expr ) :: mid :: Product( Number( m ), d:Expr, e:Expr, f:Expr ) :: end ) if ( a == d && b == e && c == f ) => Sum( Product( Number( n + m ), a, b, c ) :: mid :: end ).visit()
      case ( Product( a:Expr, b:Expr, c:Expr ) :: mid :: Product( d:Expr, e:Expr, f:Expr ) :: end ) if ( a == d && b == e && c == f) => Sum( Product( Number( 2 ), a, b, c ) :: mid :: end ).visit()
      
      case ( start :: Product( Number( n ), a:Expr, b:Expr ) :: Product( c:Expr, d:Expr ) :: end ) if ( a == c && b == d ) => Sum( start :: Product( Number( n + 1 ), a, b ) :: end ).visit()
      case ( start :: Product( a:Expr, b:Expr ) :: Product( Number( m ), c:Expr, d:Expr ) :: end ) if ( a == c && b == d ) => Sum( start :: Product( Number( 1 + m ), a, b ) :: end ).visit()
      case ( start :: Product( Number( n ), a:Expr, b:Expr ) :: Product( Number( m ), c:Expr, d:Expr ) :: end ) if ( a == c && b == d ) => Sum( start :: Product( Number( n + m ), a, b ) :: end ).visit()
      case ( start :: Product( Number( n ), a:Expr, b:Expr, c:Expr ) :: Product( Number( m ), d:Expr, e:Expr, f:Expr ) :: end ) if ( a == d && b == e && c == f) => Sum( start :: Product( Number( n + m ), a, b, c ) :: end ).visit()
      case ( start :: Product( a:Expr, b:Expr, c:Expr ) :: Product( d:Expr, e:Expr, f:Expr ) :: end ) if ( a == d && b == e && c == f) => Sum( start :: Product( Number( 2 ), a, b, c ) :: end ).visit()
  

      /*
      case ( start :: Product( Number( n ), a:Expr, b:Expr ) :: mid :: Product( c:Expr, d:Expr ) :: Nil ) if ( a == c && b == d ) => Sum( start :: Product( Number( n + 1 ), a, b ) :: mid ).visit()
      case ( start :: Product( a:Expr, b:Expr ) :: mid :: Product( Number( m ), c:Expr, d:Expr ) :: Nil ) if ( a == c && b == d ) => Sum( start :: Product( Number( 1 + m ), a, b ) :: mid ).visit()
      case ( start :: Product( Number( n ), a:Expr, b:Expr ) :: mid :: Product( Number( m ), c:Expr, d:Expr ) ) if ( a == c && b == d ) => Sum( start :: Product( Number( n + m ), a, b ) :: mid  ).visit()
      case ( start :: Product( Number( n ), a:Expr, b:Expr, c:Expr ) :: mid :: Product( Number( m ), d:Expr, e:Expr, f:Expr ) ) if ( a == d && b == e && c == f) => Sum( start :: Product( Number( n + m ), a, b, c ) :: mid ).visit()
      case ( start :: Product( a:Expr, b:Expr, c:Expr ) :: mid :: Product( d:Expr, e:Expr, f:Expr ) ) if ( a == d && b == e && c == f) => Sum( start :: Product( Number( 2 ), a, b, c ) :: mid  ).visit()
      */

// (-3.0)*sin(psi)^2.0+((-1.0)*cos(psi)^2.0+5.0*sin(psi)^2.0+1.0 -> 0
      //case ( Product( Number( -3, Power( SinF1( psi1:Expr), Number( 2 ) ) ) ) ::
      //    )
  
      case ( Product( Number( -1 ), Power( CosF1( psi1:Expr ), Number( 2 ) ) ) :: Power( SinF1( psi2:Expr ), Number( 2 ) ) :: Number( 1 ) :: Nil ) if ( psi1 == psi2 ) => Product( Number( 2 ), Power( SinF1( psi1:Expr ), Number( 2 ) ) )      
      case ( Product( Number( -1 ), Power( CosF1( psi1:Expr ), Number( 2 ) ) ) :: Product( Number( n ), Power( SinF1( psi2:Expr ), Number( 2 ) ) ) :: Number( 1 ) :: Nil ) if ( psi1 == psi2 && n > 0 ) => Product( Number( n+1 ), Power( SinF1( psi1:Expr ), Number( 2 ) ) )      
      case ( Product( Number( -2 ), Power( CosF1( psi1:Expr ), Number( 2 ) ) ) :: Product( Number( n ), Power( SinF1( psi2:Expr ), Number( 2 ) ) ) :: Number( 2 ) :: Nil ) if ( psi1 == psi2 && n > 0 ) => Product( Number( n+2 ), Power( SinF1( psi1:Expr ), Number( 2 ) ) )      
      //-1.0*sin(psi)^2.0*cos(theta)^2.0-2.0*sin(psi)^2.0*sin(theta)^2.0+sin(psi)^2.0 -> -1.0*sin(psi)^2.0*sin(theta)^2.0
      case ( 
        Product( Number( -1 ), Power(  SinF1(psi1:Expr ), Number( 2 ) ), Power( CosF1( theta1:Expr ), Number( 2 ) ) ) ::
        Product( Number( -2 ), Power(  SinF1(psi2:Expr ), Number( 2 ) ), Power( SinF1( theta2:Expr ), Number( 2 ) ) ) ::
        Power( SinF1(psi3:Expr ), Number( 2 ) ) :: Nil ) if ( psi1 == psi2 && psi1 == psi3 && theta1 == theta2 ) => Product( Number( -1 ), Power(  SinF1(psi1:Expr ), Number( 2 ) ), Power( SinF1( theta1:Expr ), Number( 2 ) ) )
      case ( 
        start ::
        Product( Number( -1 ), Power( SinF1(psi1:Expr ), Number( 2 ) ), Power( CosF1( theta1:Expr ), Number( 2 ) ) ) ::
        Product( Number( -2 ), Power( SinF1(psi2:Expr ), Number( 2 ) ), Power( SinF1( theta2:Expr ), Number( 2 ) ) ) ::
        Power( SinF1(psi3:Expr ), Number( 2 ) ) ::
        end ) if ( psi1 == psi2 && psi1 == psi3 && theta1 == theta2 ) => Sum( start :: Product( Number( -1 ), Power( SinF1(psi1:Expr ), Number( 2 ) ), Power( SinF1( theta1:Expr ), Number( 2 ) ) ) :: end ).visit()
      // -1.0*sin(psi)^2.0+sin(psi)^2.0*cos(theta)^2.0-1.0*sin(psi)^2.0*sin(theta)^2.0 -> -2.0*sin(psi)^2.0*sin(theta)^2.0
      case ( 
        start ::
        Product( Number( -1 ), Power( SinF1(psi1:Expr ), Number( 2 ) ) ) ::
        Product(               Power( SinF1(psi2:Expr ), Number( 2 ) ), Power( CosF1( theta1:Expr ), Number( 2 ) ) ) ::
        Product( Number( -1 ), Power( SinF1(psi3:Expr ), Number( 2 ) ), Power( SinF1( theta2:Expr ), Number( 2 ) ) ) ::
        end ) if ( psi1 == psi2 && psi1 == psi3 && theta1 == theta2 ) => Sum( start :: Product( Number( -2 ), Power( SinF1(psi1:Expr ), Number( 2 ) ), Power( SinF1( theta1:Expr ), Number( 2 ) ) ) :: end ).visit()
     case ( 
        start ::
        Product( Number( -1 ), Power( SinF1(psi1:Expr ), Number( 2 ) ) ) ::
        Product(               Power( SinF1(psi2:Expr ), Number( 2 ) ), Power( CosF1( theta1:Expr ), Number( 2 ) ) ) ::
        Product( Number( -1 ), Power( SinF1(psi3:Expr ), Number( 2 ) ), Power( SinF1( theta2:Expr ), Number( 2 ) ) ) ::
        Nil ) if ( psi1 == psi2 && psi1 == psi3 && theta1 == theta2 ) => Sum( start :: Product( Number( -2 ), Power( SinF1(psi1:Expr ), Number( 2 ) ), Power( SinF1( theta1:Expr ), Number( 2 ) ) ) :: Nil ).visit()
     case ( 
        Product( Number( -1 ), Power( SinF1(psi1:Expr ), Number( 2 ) ) ) ::
        Product(               Power( SinF1(psi2:Expr ), Number( 2 ) ), Power( CosF1( theta1:Expr ), Number( 2 ) ) ) ::
        Product( Number( -1 ), Power( SinF1(psi3:Expr ), Number( 2 ) ), Power( SinF1( theta2:Expr ), Number( 2 ) ) ) ::
        end ) if ( psi1 == psi2 && psi1 == psi3 && theta1 == theta2 ) => Sum( Product( Number( -2 ), Power( SinF1(psi1:Expr ), Number( 2 ) ), Power( SinF1( theta1:Expr ), Number( 2 ) ) ) :: end ).visit()
     
      case _ => expressify(scan(Sum.neutralElement, ts, pairSum))
    }
  }

  // turn a list of Expr into an Expr (likely a Sum)
  def expressify(l: List[Expr]): Expr = l.filter(factor => factor != Number(0)) match {
    case Nil => Number(0)
    case a :: Nil => a
    case b => Sum(b)
  }

  // 2 * a + 2 * b -> 2 * (a + b)
  /*override def factor:Expr = this.factors match {
    case Some( e:Expr ) => e
    case None => this
  }
  */

  override def expand: Expr = {
      def expandSum(left: Expr, right: Expr): Option[Expr] = (left, right) match {
        case (Fraction(a,b), Fraction( c, d )) => Some( Fraction( Sum( Product( a, d ), Product( b, c ) ), Product( b, d ) ).visit() )
        case (Fraction(a,b),c) => Some( Fraction( Sum( a, Product( b, c ) ), b ).visit() )
        case (a,Fraction(b,c)) => Some( Fraction( Sum( Product( a, c ), b ), c ).visit() )
        case _ => None
    }
    // expand all terms
    val ts = flatTerms.map(term => term.expand ).toList
    expressify( scan(Sum.neutralElement, ts, expandSum ) )
    // bring all terms to a common denominator

    /*
    // then, to the extent there are fractions, put everything on a common denominator
    // a/b + c/d + e/f => ( a*d*f + c*b*f + e*b*d ) / (b*d*f)
    val denominators = rv.flatTerms.collect( { case Fraction(_,d) => d } )
    val nt = rv.flatTerms.map( {
      case Fraction( n, d ) => Product( n, Product( denominators.filter( denominator => denominator != d ) ) ).visit()
      case e => Product( e, Product( denominators ) ).visit()
    } )

    Fraction( Sum( nt ), Product( denominators ).expand ).visit()
    */
  }

  override def possibleFactors = List( this ) ++ terms(0).possibleFactors
  
  // a*b+a+a*c+d*a+d*b*a -> a*(1+b+c+d*(1+b))
  override def factor: Expr = {
    //( "sum.factor(" + this + ")" )

    // search based factorization
    def searchFactor(a: Expr, b: Expr): Option[Expr] = {
      if (a.possibleFactors.size == 0)
        return None
      if (b.possibleFactors.size == 0)
        return None

      val pfs: List[Expr] = a.possibleFactors ++ b.possibleFactors

      var i = 0
      var found = false
      var rv: Option[Expr] = None
      while (!found && i < pfs.size) {
        val pf = pfs(i)

        (a.extractFactor(pf), b.extractFactor(pf)) match {
          case (Some(ra), Some(rb)) => {
            found = true
            require(Product(pf, Sum(ra, rb)).factors.size < 5)
            require(Sum(ra, rb).flatTerms.size < 5)
            //println( "Sum.factor: rv:" + Product( pf, Sum( ra, rb ) ) )

            rv = Some(Product(pf, Sum(ra, rb).visit().factor)) // adding recursion here creates an inf. loop - don't know why yet
          }
          case _ => i = i + 1
        }
      }

      rv
    }

    // factorize all factors first
    var ts = Sum(this.terms.map(term => term.factor): _*).flatTerms.sortWith(Sum.sort)
    expressify(scan(Sum.neutralElement, ts, searchFactor))
  }

  // (a*b+a*c).extractFactor(a) -> b+c
  override def extractFactor(possibleFactor: Expr): Option[Expr] = {
    if( possibleFactor == this )
      return Some( Number( 1 ) )


    // extract sin^2 from -2cos^2+2sin^2+2
    ( possibleFactor, this ) match {
      case ( Power( SinF1(psi:Expr), Number( 2 ) ), Sum( Product( Number( -2 ), Power( CosF1( psi2:Expr ), Number( 2 ) ), Product( Number( 2 ), Power( SinF1( psi3:Expr ), Number( 2 ) ) ) ) ) ) if( psi == psi2 && psi == psi3 ) =>
        Some( Product( Number(2),Power( SinF1(psi:Expr), Number( 2 ))) )
      case _ => {

        val extracted = terms.foldLeft((List[Expr](), true))({ case ((l: List[Expr], good: Boolean), e: Expr) => if (good) {
          e.extractFactor(possibleFactor) match {
            case Some(e) => (l :+ e, true)
            case _ => (l, false)
          }
        }
        else
          (l, false)
        })

        extracted match {
          case (l, true) => return Some(Sum(l).visit())
          case (_, false) => None // do nothing
        }
      }
    }
  }
}
