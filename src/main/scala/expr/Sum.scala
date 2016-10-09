package galileo.expr

import galileo.complex._
import galileo.constants._
import galileo.environment.Environment
import galileo.linalg._
import galileo.manipulate.Simplify
import galileo.proof.Conversion
import galileo.trigonometry.{CosF1, SinF1}

object Diff {
  def apply(a: Expr, b: Expr) = Sum(a, Product(Number(-1), b))
}

object Sum {
  val sort = { (a: Expr, b: Expr) => (a, b) match {
    case (c: Complex, d: Complex) => false
    case (c: Constant, d: Constant) => c.shortName < d.shortName
    case (c: Constant, d: Complex) => false
    case (c: Constant, d: Expr) => true
    //case ( c:ConstantE, d:ConstantPi ) => true // handled by comparing shortname
    //case ( c:ConstantPi, d:ConstantE ) => false
    case (c: Complex, d: Number) => false // numbers go before complex numbers
    case (c: Complex, d: Expr) => true
    case (Number(x), Number(y)) => x < y
    case (Number(x), Variable(y)) => false // numbers after variables
    case (Number(x), _: Expr) => false // numbers after expressions
    case (_: Expr, _: Number) => true
    case (Product(_: Number, a: Variable), Product(_: Number, b: Variable)) => a < b
    case (_1, _2) => _1 < _2

    /*
        case (AtrigF1,TrigF1) => true
        case (TrigF1(x),ATrigF1(y)) => false
        case (CosF1(x,SinF1) => true
        case (SinF1(_),CosF1(_)) => false
        case (Power(x:FunF1),y:FunF1) => sort( x, y )
    */
    //case (Product())
    // cos(x)^2 before sin(x)^2
    //case (Power(SinF1:))

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

  override def simplify:Expr = Sum(flatTerms.map(term => Simplify(term).visit()).toList).visit() match {
    case s:Sum if ( s == this ) => s
    case e => e.simplify // OK to recurse
  }

  // todo, rewrite using 'scan'
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

      case (Product(Number(x), Power(CosF1(a), Number(2))), Product(Number(y), Power(SinF1(b), Number(2)))) if (a == b && x <= -1 && y <= -1) =>
        Some(Sum(Product(Number(x + 1), Power(CosF1(a), Number(2))), Product(Number(y + 1), Power(SinF1(b), Number(2))), Number(-1)))

      case (Product(Number(x), Power(CosF1(a), Number(2)),b), Product(Number(y), Power(SinF1(c), Number(2)),d)) if (a == c && b == d && x <= -1 && y <= -1) =>
        Some(Sum(Product(Number(x + 1), Power(CosF1(a), Number(2)),b), Product(Number(y + 1), Power(SinF1(c), Number(2)),d), Product( d, Number(-1))) )

      case (Product(Number(x), Power(CosF1(a), Number(2))), Product(Number(y), Power(SinF1(b), Number(2)))) if (a == b && x >= 1 && y >= 1) =>
        Some(Sum(Product(Number(x - 1), Power(CosF1(a), Number(2))), Product(Number(y - 1), Power(SinF1(b), Number(2))), Number(1)))

      //case (Power(SinF1(a),Number(2)),Product(Number(-1),Power(CosF1(b),Number(2))) if ( a == b ) => Some( Number( 1 ) )
      //case (Product( a, b), Fraction( c, d ) ) if ( a == c && b == Power( d, Number( -1 ) ) ) => Product( Number( 2 ), a, b )
      // Nice application of pattern matching to help with factorization
      // 7 * a * b + 3 * a * b -> 10 * a * b
      case (p1: Product, p2: Product) => (p1.factors.toList, p2.factors) match {
        // Need to also simplify things like a * sin(b)^2+ a * cos( b ) ^ 2
        //case ( a :: b :: c, d :: e :: f)
        case (a :: Power(CosF1(b), Number(2)) :: c, d :: Power(SinF1(e), Number(2)) :: f) if (a == d && b == e && c == f) => Some(Product(c :+ a))
        case (a :: Power(SinF1(b), Number(2)) :: c, d :: Power(CosF1(e), Number(2)) :: f) if (a == d && b == e && c == f) => Some(Product(c :+ a))
        case (a :: Power(CosF1(b), Number(2)) :: Nil, d :: Power(SinF1(e), Number(2)) :: Nil) if (a == d && b == e) => Some(Product(a))
        case (a :: Power(SinF1(b), Number(2)) :: Nil, d :: Power(CosF1(e), Number(2)) :: Nil) if (a == d && b == e) => Some(Product(a))
        case (a :: b :: Power(CosF1(c), Number(2)) :: d, e :: f :: Power(SinF1(g), Number(2)) :: h) if (a == e && b == f && c == g && d == h) => Some(Product(d :+ a :+ b))
        case (a :: b :: Power(SinF1(c), Number(2)) :: d, e :: f :: Power(CosF1(g), Number(2)) :: h) if (a == e && b == f && c == g && d == h) => Some(Product(d :+ a :+ b))
        case (a :: b :: c :: Power(CosF1(d), Number(2)) :: e, f :: g :: h :: Power(SinF1(i), Number(2)) :: j) if (a == f && b == g && c == h && d == i && e == j) => Some(Product(e :+ a :+ b :+ c))
        case (a :: b :: c :: Power(SinF1(d), Number(2)) :: e, f :: g :: h :: Power(CosF1(i), Number(2)) :: j) if (a == f && b == g && c == h && d == i && e == j) => Some(Product(e :+ a :+ b :+ c))


        case (a, c :: d) if (a == d) => Some(Product(Sum(Number(1), c) :: a).visit())
        case (a :: b, c) if (b == c) => Some(Product(Sum(Number(1), a) :: b).visit())
        case (a :: b, c :: d) if (b == d) => Some(Product(Sum(a, c) :: b).visit())
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
    expressify(scan(Sum.neutralElement, ts, pairSum))
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
    val ts = flatTerms.map(term => term.expand).toList
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
