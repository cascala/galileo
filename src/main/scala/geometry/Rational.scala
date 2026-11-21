package galileo.geometry

import galileo.constants._
import galileo.expr._
import galileo.trigonometry._

import scala.collection.mutable

// Rational function normal form, used to simplify curvature results.
//
// An expression is written as  numerator / (f1^k1 * f2^k2 * ...)  where the numerator
// and the factors fi are expanded polynomials in "kernels" (variables and function
// applications such as sin(theta)). Sums are put over a common denominator, the
// numerator is divided exactly by the denominator factors wherever possible, and
// sin^2 + cos^2 = 1 is applied in one direction. This decides zero (e.g. the Ricci
// tensor of the Schwarzschild metric) and cancels common factors.
class Rational( trig: Rational.Trig ) {
  import Rational._

  private val kernels = mutable.ArrayBuffer[ Expr ]()
  private val kernelIds = mutable.HashMap[ Expr, Int ]()

  private def kernel( e: Expr ): Int = kernelIds.getOrElseUpdate( e, { kernels += e; kernels.size - 1 } )

  // ---- polynomials: monomial (sorted (kernel, exponent) pairs) -> coefficient
  type Mono = Vector[ ( Int, Int ) ]
  type Poly = Map[ Mono, Double ]

  private val one: Mono = Vector()
  private def const( c: Double ): Poly = if ( c == 0 ) Map() else Map( one -> c )
  private def kpoly( k: Int ): Poly = Map( Vector( ( k, 1 ) ) -> 1.0 )

  private def mulMono( a: Mono, b: Mono ): Mono = {
    val m = mutable.TreeMap[ Int, Int ]()
    a.foreach { case ( k, e ) => m( k ) = m.getOrElse( k, 0 ) + e }
    b.foreach { case ( k, e ) => m( k ) = m.getOrElse( k, 0 ) + e }
    m.filter( _._2 != 0 ).toVector
  }

  // drop numerical noise and round to 12 significant digits, so that equal
  // polynomials computed in different ways compare equal
  private def clean( p: mutable.Map[ Mono, Double ] ): Poly = {
    val scale = if ( p.isEmpty ) 0.0 else p.values.map( math.abs ).max
    p.filter { case ( _, c ) => math.abs( c ) > 1e-11 * scale && c != 0 }.map { case ( m, c ) => ( m, round( c ) ) }.toMap
  }

  private def add( a: Poly, b: Poly, s: Double = 1.0 ): Poly = {
    val m = mutable.HashMap[ Mono, Double ]() ++= a
    b.foreach { case ( mo, c ) => m( mo ) = m.getOrElse( mo, 0.0 ) + s * c }
    reduceTrig( clean( m ) )
  }

  private def mul( a: Poly, b: Poly ): Poly = {
    val m = mutable.HashMap[ Mono, Double ]()
    for ( ( ma, ca ) <- a; ( mb, cb ) <- b ) { val mo = mulMono( ma, mb ); m( mo ) = m.getOrElse( mo, 0.0 ) + ca * cb }
    reduceTrig( clean( m ) )
  }

  private def pow( a: Poly, k: Int ): Poly = ( 1 to k ).foldLeft( const( 1 ) )( ( acc, _ ) => mul( acc, a ) )

  // sin(x)^2 -> 1 - cos(x)^2 (or the reverse), when the partner kernel exists
  private def reduceTrig( p: Poly ): Poly = {
    if ( trig == Trig.None ) return p
    var changed = false
    val m = mutable.HashMap[ Mono, Double ]()
    def put( mo: Mono, c: Double ) = m( mo ) = m.getOrElse( mo, 0.0 ) + c
    for ( ( mo, c ) <- p ) {
      val hit = mo.find { case ( k, e ) => e >= 2 && partner( k ).isDefined }
      hit match {
        case Some( ( k, e ) ) => {
          changed = true
          val q = partner( k ).get
          val rest = mo.filter( _._1 != k ) ++ ( if ( e > 2 ) Vector( ( k, e - 2 ) ) else Vector() )
          val restS = rest.sortBy( _._1 )
          put( restS, c ) // * 1
          put( mulMono( restS, Vector( ( q, 2 ) ) ), -c ) // * -partner^2
        }
        case None => put( mo, c )
      }
    }
    if ( changed ) reduceTrig( clean( m ) ) else p
  }

  private def partner( k: Int ): Option[ Int ] = ( trig, kernels( k ) ) match {
    case ( Trig.SinToCos, SinF1( x ) ) => Some( kernel( CosF1( x ) ) )
    case ( Trig.CosToSin, CosF1( x ) ) => Some( kernel( SinF1( x ) ) )
    case _ => None
  }

  // ---- monomial order (lex on kernel ids) and exact division
  private def cmpMono( a: Mono, b: Mono ): Int = {
    val n = math.max( a.size, b.size )
    var i = 0
    while ( i < n ) {
      if ( i >= a.size ) return -1
      if ( i >= b.size ) return 1
      val ( ka, ea ) = a( i ); val ( kb, eb ) = b( i )
      if ( ka != kb ) return if ( ka < kb ) 1 else -1
      if ( ea != eb ) return if ( ea > eb ) 1 else -1
      i += 1
    }
    0
  }
  private def leading( p: Poly ): ( Mono, Double ) = p.maxBy( _._1 )( Ordering.fromLessThan( ( a, b ) => cmpMono( a, b ) < 0 ) )

  private def divMono( a: Mono, b: Mono ): Option[ Mono ] = {
    val am = a.toMap
    if ( b.exists { case ( k, e ) => am.getOrElse( k, 0 ) < e } ) None
    else Some( mulMono( a, b.map { case ( k, e ) => ( k, -e ) } ) )
  }

  // p / d if d divides p exactly (no trig reduction during division)
  private def divide( p: Poly, d: Poly ): Option[ Poly ] = {
    if ( d.isEmpty ) return None
    val ( ld, cd ) = leading( d )
    var rem = p
    val q = mutable.HashMap[ Mono, Double ]()
    var guard = 0
    while ( rem.nonEmpty ) {
      guard += 1
      if ( guard > 100000 ) return None
      val ( lr, cr ) = leading( rem )
      divMono( lr, ld ) match {
        case None => return None
        case Some( mo ) => {
          val c = cr / cd
          q( mo ) = q.getOrElse( mo, 0.0 ) + c
          val m = mutable.HashMap[ Mono, Double ]() ++= rem
          for ( ( md, cdd ) <- d ) { val x = mulMono( mo, md ); m( x ) = m.getOrElse( x, 0.0 ) - c * cdd }
          m.remove( lr )
          // drop numerical noise relative to the size of p
          val scale = p.values.map( math.abs ).max
          rem = m.filter { case ( _, v ) => math.abs( v ) > 1e-9 * scale }.toMap
        }
      }
    }
    Some( q.toMap )
  }

  // ---- rational functions
  case class Rat( num: Poly, den: List[ ( Poly, Int ) ] )

  // normalise a denominator polynomial: split off its monomial and numeric content,
  // make the leading coefficient 1. Returns (numeric factor, factors)
  private def factorDen( p: Poly ): ( Double, List[ ( Poly, Int ) ] ) = {
    if ( p.isEmpty ) throw new ArithmeticException( "division by zero" )
    // monomial content: kernels dividing every term
    val ks = p.keys.map( _.map( _._1 ).toSet ).reduce( _ intersect _ )
    val content = ks.toVector.sorted.map( k => ( k, p.keys.map( mo => mo.find( _._1 == k ).get._2 ).min ) )
    val rest0 = p.map { case ( mo, c ) => ( divMono( mo, content ).get, c ) }
    val ( _, lc ) = leading( rest0 )
    val rest = rest0.map { case ( mo, c ) => ( mo, round( c / lc ) ) }
    val fs = content.map { case ( k, e ) => ( kpoly( k ), e ) }.toList ++ ( if ( rest == const( 1 ) ) Nil else List( ( rest, 1 ) ) )
    ( lc, fs )
  }

  private def merge( a: List[ ( Poly, Int ) ], b: List[ ( Poly, Int ) ] ): List[ ( Poly, Int ) ] = {
    val m = mutable.LinkedHashMap[ Poly, Int ]()
    ( a ++ b ).foreach { case ( p, k ) => m( p ) = m.getOrElse( p, 0 ) + k }
    m.filter( _._2 != 0 ).toList
  }

  private def ratMul( a: Rat, b: Rat ): Rat = Rat( mul( a.num, b.num ), merge( a.den, b.den ) )

  private def ratAdd( a: Rat, b: Rat ): Rat = {
    // common denominator: maximum power of each factor
    val m = mutable.LinkedHashMap[ Poly, Int ]()
    ( a.den ++ b.den ).foreach { case ( p, k ) => m( p ) = math.max( m.getOrElse( p, 0 ), k ) }
    val common = m.toList
    def lift( r: Rat ) = {
      val have = r.den.toMap
      common.foldLeft( r.num ) { case ( acc, ( p, k ) ) => mul( acc, pow( p, k - have.getOrElse( p, 0 ) ) ) }
    }
    Rat( add( lift( a ), lift( b ) ), common )
  }

  private def ratInv( a: Rat ): Rat = {
    val ( c, fs ) = factorDen( a.num )
    val num = a.den.foldLeft( const( 1.0 / c ) ) { case ( acc, ( p, k ) ) => mul( acc, pow( p, k ) ) }
    Rat( num, fs )
  }

  private def ratPow( a: Rat, k: Int ): Rat =
    if ( k >= 0 ) ( 1 to k ).foldLeft( Rat( const( 1 ), Nil ) )( ( acc, _ ) => ratMul( acc, a ) )
    else ratInv( ratPow( a, -k ) )

  private def atom( e: Expr ): Rat = Rat( kpoly( kernel( e ) ), Nil )

  // polynomials seen as sums in the input: candidates for common factors
  private val candidates = mutable.LinkedHashSet[ Poly ]()

  private def remember( r: Rat ): Rat = {
    if ( r.den.isEmpty && r.num.size > 1 ) {
      val ( _, fs ) = factorDen( r.num )
      fs.foreach { case ( p, _ ) => if ( p.size > 1 ) candidates += p }
    }
    r
  }

  def toRat( e: Expr ): Rat = e match {
    case Number( v ) => Rat( const( v ), Nil )
    case NumberI( v ) => Rat( const( v.toDouble ), Nil )
    case s: Sum => remember( s.terms.map( toRat ).reduceOption( ratAdd ).getOrElse( Rat( Map(), Nil ) ) )
    case p: Product => p.factors.map( toRat ).foldLeft( Rat( const( 1 ), Nil ) )( ratMul )
    case Fraction( a, b ) => ratMul( toRat( a ), ratInv( toRat( b ) ) )
    case Power( a, Number( k ) ) if k == math.rint( k ) && math.abs( k ) <= 64 => ratPow( toRat( a ), k.toInt )
    case Power( a, b ) => atom( Power( simplifyInner( a ), simplifyInner( b ) ) )
    case SinF1( a ) => atom( SinF1( simplifyInner( a ) ) )
    case CosF1( a ) => atom( CosF1( simplifyInner( a ) ) )
    case TanF1( a ) => atom( TanF1( simplifyInner( a ) ) )
    case LogF1( a ) => atom( LogF1( simplifyInner( a ) ) )
    case ExpF1( a ) => atom( ExpF1( simplifyInner( a ) ) )
    case v: Variable => atom( Variable( v.name.trim ) )
    case other => atom( other )
  }

  private def simplifyInner( e: Expr ): Expr = e match {
    case _: Number | _: Variable => e
    case _ => new Rational( Trig.None ).simplify( e )
  }

  // cancel denominator factors that divide the numerator
  def reduce( r0: Rat ): Rat = {
    var num0 = r0.num
    // split denominator factors further using candidate factors that divide them
    val split = r0.den.flatMap { case ( p, k ) =>
      var rest = p
      val found = mutable.ListBuffer[ ( Poly, Int ) ]()
      for ( c <- candidates if c != rest && rest.size > 1 ) {
        var go = true
        while ( go ) divide( rest, c ) match {
          case Some( q ) if q.nonEmpty => { found += ( ( c, k ) ); rest = q; if ( q.size == 1 && q.head._1.isEmpty ) go = false }
          case _ => go = false
        }
      }
      if ( found.isEmpty ) List( ( p, k ) )
      else {
        // what is left is a constant times maybe a polynomial
        val ( c, fs ) = if ( rest.size == 1 && rest.head._1.isEmpty ) ( rest.head._2, Nil ) else factorDen( rest )
        num0 = mul( num0, const( math.pow( 1.0 / c, k ) ) )
        found.toList ++ fs.map { case ( q, e ) => ( q, e * k ) }
      }
    }
    val r = Rat( num0, merge( split, Nil ) )
    var num = r.num
    val den = r.den.map { case ( p, k ) =>
      var left = k
      var go = true
      while ( left > 0 && go ) divide( num, p ) match {
        case Some( q ) => { num = q; left -= 1 }
        case None => go = false
      }
      ( p, left )
    }.filter( _._2 > 0 )
    Rat( num, den )
  }

  // ---- back to expressions
  private def monoExpr( mo: Mono ): List[ Expr ] = mo.toList.map { case ( k, e ) => if ( e == 1 ) kernels( k ) else Power( kernels( k ), Number( e ) ) }

  private def polyExpr( p: Poly ): Expr = {
    if ( p.isEmpty ) return Number( 0 )
    val terms = p.toList.sortWith( ( a, b ) => cmpMono( a._1, b._1 ) > 0 ).map { case ( mo, c ) =>
      val fs = monoExpr( mo )
      ( c, fs ) match {
        case ( c, Nil ) => Number( c )
        case ( 1.0, List( f ) ) => f
        case ( 1.0, many ) => Product( many: _* )
        case ( c, many ) => Product( ( Number( c ) :: many ): _* )
      }
    }
    if ( terms.size == 1 ) terms.head else orderTerms( Sum( terms: _* ) )
  }

  // let Galileo put terms and factors in its usual order (e.g. R+a*cos(u))
  private def orderTerms( s: Sum ): Expr = try {
    val v = s.visit()
    if ( v.toString.length <= s.toString.length + 2 ) v else s
  } catch { case _: Throwable => s }

  // write the numerator with its monomial and numeric content factored out
  private def numExpr( p: Poly ): ( Double, Expr ) = {
    if ( p.isEmpty ) return ( 0.0, Number( 0 ) )
    if ( p.size == 1 ) { val ( mo, c ) = p.head; return ( c, if ( mo.isEmpty ) Number( 1 ) else polyExpr( Map( mo -> 1.0 ) ) ) }
    val ks = p.keys.map( _.map( _._1 ).toSet ).reduce( _ intersect _ )
    val content = ks.toVector.sorted.map( k => ( k, p.keys.map( mo => mo.find( _._1 == k ).get._2 ).min ) )
    val rest = p.map { case ( mo, c ) => ( divMono( mo, content ).get, c ) }
    // numeric content: make the leading coefficient positive, and integers where possible
    val ( _, lc ) = leading( rest )
    val g0 = if ( lc < 0 ) -1.0 else 1.0
    // a common numeric factor such as 0.5 in 0.5*r - 0.5*r_s
    val g = if ( rest.values.forall( c => integral( c / math.abs( lc ) ) ) ) g0 * math.abs( lc ) else g0
    val restN = rest.map { case ( mo, c ) => ( mo, c / g ) }
    val inner = polyExpr( restN )
    val factors = monoExpr( content ) :+ inner
    ( g, if ( factors.size == 1 ) factors.head else Product( factors: _* ) )
  }

  private def integral( x: Double ) = math.abs( x - math.rint( x ) ) < 1e-9 * math.max( 1.0, math.abs( x ) )

  def toExpr( r0: Rat ): Expr = {
    // scale denominator factors to integer coefficients: x^2+0.25 -> 4*x^2+1
    var scale = 1.0
    val dens = r0.den.map { case ( p, k ) =>
      ( 1 to 64 ).find( m => p.values.forall( c => integral( c * m ) ) ) match {
        case Some( m ) if m > 1 => { scale *= math.pow( m, k ); ( p.map { case ( mo, c ) => ( mo, math.rint( c * m ) ) }, k ) }
        case _ => ( p, k )
      }
    }
    val r = Rat( r0.num.map { case ( mo, c ) => ( mo, round( c * scale ) ) }, dens )
    val ( c, n ) = numExpr( r.num )
    if ( c == 0 ) return Number( 0 )
    // clear fractional coefficients: find small integer m with c*m integral
    val m = ( 1 to 64 ).find( k => math.abs( c * k - math.rint( c * k ) ) < 1e-9 ).getOrElse( 1 )
    val cn = math.rint( c * m * 1e9 ) / 1e9
    val denFactors = r.den.map { case ( p, k ) => val pe = polyExpr( p ); if ( k == 1 ) pe else Power( pe, Number( k ) ) }
    val den = ( if ( m != 1 ) List( Number( m ) ) else Nil ) ++ denFactors
    val numE = ( cn, n ) match {
      case ( 1.0, x ) => x
      case ( x, Number( 1.0 ) ) => Number( x )
      case ( x, p: Product ) => Product( ( Number( x ) :: p.factors.toList ): _* )
      case ( x, y ) => Product( Number( x ), y )
    }
    den match {
      case Nil => numE
      case List( d ) => Fraction( numE, d )
      case ds => Fraction( numE, Product( ds: _* ) )
    }
  }

  def simplify( e: Expr ): Expr = toExpr( reduce( toRat( e ) ) )
}

object Rational {
  def round( c: Double ): Double = if ( c == 0 || c.isNaN || c.isInfinite ) c else {
    val mag = math.pow( 10, 11 - math.floor( math.log10( math.abs( c ) ) ) )
    math.rint( c * mag ) / mag
  }

  sealed trait Trig
  object Trig {
    case object None extends Trig
    case object SinToCos extends Trig
    case object CosToSin extends Trig
  }

  // try the trigonometric rewrites in both directions and keep the shortest result
  def simplify( e: Expr ): Expr = {
    val candidates = List( Trig.None, Trig.CosToSin, Trig.SinToCos ).flatMap { t =>
      try Some( new Rational( t ).simplify( e ) ) catch { case _: Throwable => None }
    }
    if ( candidates.isEmpty ) e else candidates.minBy( c => Tidy.size( c ) )
  }
}
