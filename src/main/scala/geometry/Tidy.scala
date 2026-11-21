package galileo.geometry

import galileo.expr._
import galileo.manipulate.Simplify

// Simplification used for differential geometry results.
// Runs Galileo's simplifier, then cancels common powers between numerator and
// denominator (e.g. 2*r^12*sin(theta)^6/(r^14*sin(theta)^6) -> 2/r^2), which the
// general simplifier does not always do.
object Tidy {
  def apply( e: Expr ): Expr = {
    val r = try Rational.simplify( e ) catch { case _: Throwable => e }
    if ( size( r ) <= size( e ) ) r else e
  }

  // the older approach based on Galileo's general simplifier
  def legacy( e: Expr ): Expr = {
    val once = step( e )
    val twice = step( once )
    if ( size( twice ) <= size( once ) ) twice else once
  }

  private def step( e: Expr ): Expr = {
    val s = try Simplify( e ).visit() catch { case _: Throwable => e }
    try cancel( s ) catch { case _: Throwable => s }
  }

  // printed length, with a penalty for non-integer coefficients such as 0.5
  // and for each sum (so -sin(x)^2 is preferred over cos(x)^2-1)
  def size( e: Expr ): Int = {
    val s = e.toString
    s.length + 4 * "\\.[0-9]*[1-9]".r.findAllIn( s ).size + 4 * "(?<![(eE])[-+]".r.findAllIn( s ).size
  }

  // factor list -> (coefficient, base -> exponent)
  private def monomial( e: Expr ): ( Double, List[ ( Expr, Double ) ] ) = {
    var coef = 1.0
    val acc = scala.collection.mutable.LinkedHashMap[ Expr, Double ]()
    def add( b: Expr, k: Double ) = acc( b ) = acc.getOrElse( b, 0.0 ) + k
    val factors = e match {
      case p: Product => p.factors.toList.flatMap( _.flatFactors )
      case other => List( other )
    }
    factors.foreach {
      case Number( v ) => coef *= v
      case Power( b, Number( k ) ) => add( b, k )
      case f => add( f, 1.0 )
    }
    ( coef, acc.toList )
  }

  private def rebuild( coef: Double, fs: List[ ( Expr, Double ) ] ): Expr = {
    val parts = fs.filter( _._2 != 0 ).map {
      case ( b, 1.0 ) => b
      case ( b, k ) => Power( b, Number( k ) )
    }
    ( coef, parts ) match {
      case ( c, Nil ) => Number( c )
      case ( 1.0, List( one ) ) => one
      case ( 1.0, many ) => Product( many: _* )
      case ( c, many ) => Product( ( Number( c ) :: many ): _* )
    }
  }

  def cancel( e: Expr ): Expr = e match {
    case Fraction( n0, d0 ) => {
      val ( n, d ) = ( cancel( n0 ), cancel( d0 ) )
      val ( cn, fn ) = monomial( n )
      val ( cd, fd ) = monomial( d )
      if ( cd == 0 ) return Fraction( n, d )
      val dm = fd.toMap
      val nm = fn.toMap
      // subtract common exponents
      val newN = fn.map { case ( b, k ) => dm.get( b ) match {
        case Some( kd ) => ( b, math.max( 0.0, k - kd ) )
        case None => ( b, k )
      } }
      val newD = fd.map { case ( b, k ) => nm.get( b ) match {
        case Some( kn ) => ( b, math.max( 0.0, k - kn ) )
        case None => ( b, k )
      } }
      val c = cn / cd
      val num = rebuild( c, newN )
      val den = rebuild( 1.0, newD )
      den match {
        case Number( 1.0 ) => num.visit()
        case _ => Fraction( num, den ).visit()
      }
    }
    case s: Sum => Sum( s.terms.map( cancel ): _* ).visit()
    case p: Product => Product( p.factors.map( cancel ): _* ).visit()
    case Power( a, b ) => Power( cancel( a ), b ).visit()
    case other => other
  }
}
