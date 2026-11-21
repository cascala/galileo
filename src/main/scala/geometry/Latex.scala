package galileo.geometry

import galileo.constants._
import galileo.expr._
import galileo.trigonometry._

// LaTeX rendering of expressions, used for rich (MathJax) output in notebooks
object Latex {
  private val greek = Set( "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota", "kappa",
    "lambda", "mu", "nu", "xi", "pi", "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega",
    "Gamma", "Delta", "Theta", "Lambda", "Xi", "Pi", "Sigma", "Upsilon", "Phi", "Psi", "Omega" )

  def name( n: String ): String = {
    val trimmed = n.trim
    val ( base, sub ) = trimmed.indexOf( '_' ) match {
      case -1 => ( trimmed, "" )
      case i => ( trimmed.take( i ), trimmed.drop( i + 1 ) )
    }
    val b = if ( greek( base ) ) "\\" + base else if ( base.length > 1 ) "\\mathrm{" + base + "}" else base
    if ( sub.isEmpty ) b else b + "_{" + ( if ( greek( sub ) ) "\\" + sub else sub ) + "}"
  }

  def number( v: Double ): String =
    if ( v == math.rint( v ) && math.abs( v ) < 1e15 ) v.toLong.toString
    else {
      val s = f"$v%.6g"
      if ( s.contains( "e" ) ) { val Array( m, x ) = s.split( "e" ); m + "\\times 10^{" + x.toInt + "}" } else s
    }

  private def isNegative( e: Expr ): Boolean = e match {
    case Number( v ) => v < 0
    case p: Product => p.factors.headOption.exists { case Number( v ) => v < 0; case _ => false }
    case Fraction( n, _ ) => isNegative( n )
    case _ => false
  }

  private def negate( e: Expr ): Expr = e match {
    case Number( v ) => Number( -v )
    case p: Product => p.factors.toList match {
      case Number( -1.0 ) :: rest => if ( rest.size == 1 ) rest.head else Product( rest: _* )
      case Number( v ) :: rest => Product( ( Number( -v ) :: rest ): _* )
      case _ => p
    }
    case Fraction( n, d ) => Fraction( negate( n ), d )
    case other => other
  }

  private def atom( e: Expr ): String = e match {
    case _: Sum | _: Fraction => "\\left(" + apply( e ) + "\\right)"
    case p: Product => "\\left(" + apply( p ) + "\\right)"
    case Number( v ) if v < 0 => "\\left(" + number( v ) + "\\right)"
    case Power( _, _ ) => "\\left(" + apply( e ) + "\\right)"
    case other => apply( other )
  }

  private def factor( e: Expr ): String = e match {
    case _: Sum => "\\left(" + apply( e ) + "\\right)"
    case other => apply( other )
  }

  private def fn( name: String, a: Expr ) = "\\" + name + "\\left(" + apply( a ) + "\\right)"

  def apply( e: Expr ): String = e match {
    case Number( v ) => number( v )
    case NumberI( v ) => v.toString
    case Variable( n ) => name( n )
    case _: ConstantPi => "\\pi"
    case _: ConstantE => "e"
    case s: Sum => {
      val terms = s.terms.toList
      terms.zipWithIndex.map { case ( t, i ) =>
        if ( isNegative( t ) ) " - " + apply( negate( t ) )
        else if ( i == 0 ) apply( t ) else " + " + apply( t )
      }.mkString.stripPrefix( " " )
    }
    case p: Product => {
      val fs = p.factors.toList
      fs match {
        case Number( -1.0 ) :: rest if rest.nonEmpty => "-" + apply( if ( rest.size == 1 ) rest.head else Product( rest: _* ) )
        case _ => {
          // keep fractions as one \frac: a * b/c -> \frac{a b}{c}
          val ( fracs, others ) = fs.partition( _.isInstanceOf[ Fraction ] )
          if ( fracs.nonEmpty ) {
            val nums = others ++ fracs.map( _.asInstanceOf[ Fraction ].numerator )
            val dens = fracs.map( _.asInstanceOf[ Fraction ].denominator )
            apply( Fraction( Product( nums: _* ), Product( dens: _* ) ) )
          } else fs.filterNot( _ == Number( 1.0 ) ).map( factor ).mkString( " " )
        }
      }
    }
    case Fraction( n, d ) =>
      if ( isNegative( n ) ) "-\\frac{" + apply( negate( n ) ) + "}{" + apply( d ) + "}"
      else "\\frac{" + apply( n ) + "}{" + apply( d ) + "}"
    case Power( a, Fraction( Number( 1.0 ), Number( 2.0 ) ) ) => "\\sqrt{" + apply( a ) + "}"
    case Power( a, Number( 0.5 ) ) => "\\sqrt{" + apply( a ) + "}"
    case Power( a, Number( k ) ) if k < 0 => "\\frac{1}{" + apply( Power( a, Number( -k ) ) ) + "}"
    case Power( f @ ( SinF1( _ ) | CosF1( _ ) | TanF1( _ ) ), k ) => {
      val ( nm, arg ) = f match { case SinF1( x ) => ( "sin", x ); case CosF1( x ) => ( "cos", x ); case TanF1( x ) => ( "tan", x ); case _ => ( "", f ) }
      "\\" + nm + "^{" + apply( k ) + "}\\left(" + apply( arg ) + "\\right)"
    }
    case Power( a, b ) => atom( a ) + "^{" + apply( b ) + "}"
    case SinF1( a ) => fn( "sin", a )
    case CosF1( a ) => fn( "cos", a )
    case TanF1( a ) => fn( "tan", a )
    case AsinF1( a ) => fn( "arcsin", a )
    case AcosF1( a ) => fn( "arccos", a )
    case AtanF1( a ) => fn( "arctan", a )
    case LogF1( a ) => fn( "ln", a )
    case ExpF1( a ) => "e^{" + apply( a ) + "}"
    case Abs( a ) => "\\left|" + apply( a ) + "\\right|"
    case Derivative( y, x ) => "\\frac{d}{d" + apply( x ) + "}" + atom( y )
    case other => "\\mathrm{" + other.toString.replace( "_", "\\_" ) + "}"
  }
}
