package galileo.geometry

import galileo.constants._
import galileo.environment.Environment
import galileo.expr._
import galileo.trigonometry._

// Fast numerical evaluation of symbolic expressions in several variables.
// Used by the ODE and geodesic integrators, which evaluate the same
// expressions (right-hand sides, Christoffel symbols) many thousands of times.
object Compiled {
  type Fn = Array[ Double ] => Double

  private val maxDepth = 64

  // Compile expr into a function of the named variables (in order).
  // Other variables are looked up in env (e.g. r_s = 1 set by the user).
  def apply( expr: Expr, vars: Seq[ String ], env: Environment ): Fn = {
    val index = vars.zipWithIndex.toMap
    build( prepare( expr, index.keySet, env, 0 ), index, env )
  }

  // Substitute user-defined values for free variables and resolve derivatives once
  private def prepare( e: Expr, bound: Set[ String ], env: Environment, depth: Int ): Expr = {
    def p( s: Expr ) = prepare( s, bound, env, depth + 1 )
    if ( depth > maxDepth ) return e
    e match {
      case Variable( n ) if bound( n ) => e
      case Variable( n ) => env.get( n ) match {
        case Some( Variable( m ) ) if m == n => e
        case Some( value ) => p( value )
        case None => e
      }
      case Sum( terms @ _* ) => Sum( terms.map( p ): _* )
      case Product( factors @ _* ) => Product( factors.map( p ): _* )
      case Power( a, b ) => Power( p( a ), p( b ) )
      case Fraction( a, b ) => Fraction( p( a ), p( b ) )
      case SinF1( a ) => SinF1( p( a ) )
      case CosF1( a ) => CosF1( p( a ) )
      case TanF1( a ) => TanF1( p( a ) )
      case AsinF1( a ) => AsinF1( p( a ) )
      case AcosF1( a ) => AcosF1( p( a ) )
      case AtanF1( a ) => AtanF1( p( a ) )
      case LogF1( a ) => LogF1( p( a ) )
      case ExpF1( a ) => ExpF1( p( a ) )
      case Abs( a ) => Abs( p( a ) )
      case Derivative( y, v ) => p( Derivative( p( y ), v ).visit( Some( new Environment() ) ) )
      case other => other
    }
  }

  private def build( e: Expr, index: Map[ String, Int ], env: Environment ): Fn = {
    def b( s: Expr ) = build( s, index, env )
    e match {
      case Number( v ) => _ => v
      case NumberI( v ) => _ => v.toDouble
      case Variable( n ) if index.contains( n ) => { val i = index( n ); xs => xs( i ) }
      case Variable( "pi" ) => _ => math.Pi
      case Variable( "e" ) => _ => math.E
      case c: Constant => c.value match {
        case Number( v ) => _ => v
        case _ => _ => Double.NaN
      }
      case Sum( terms @ _* ) => {
        val fs = terms.map( b ).toArray
        xs => { var s = 0.0; var i = 0; while ( i < fs.length ) { s += fs( i )( xs ); i += 1 }; s }
      }
      case Product( factors @ _* ) => {
        val fs = factors.map( b ).toArray
        xs => { var s = 1.0; var i = 0; while ( i < fs.length ) { s *= fs( i )( xs ); i += 1 }; s }
      }
      case Power( a, Number( 2 ) ) => { val f = b( a ); xs => { val v = f( xs ); v * v } }
      case Power( a, c ) => { val ( f, g ) = ( b( a ), b( c ) ); xs => math.pow( f( xs ), g( xs ) ) }
      case Fraction( a, c ) => { val ( f, g ) = ( b( a ), b( c ) ); xs => f( xs ) / g( xs ) }
      case SinF1( a ) => { val f = b( a ); xs => math.sin( f( xs ) ) }
      case CosF1( a ) => { val f = b( a ); xs => math.cos( f( xs ) ) }
      case TanF1( a ) => { val f = b( a ); xs => math.tan( f( xs ) ) }
      case AsinF1( a ) => { val f = b( a ); xs => math.asin( f( xs ) ) }
      case AcosF1( a ) => { val f = b( a ); xs => math.acos( f( xs ) ) }
      case AtanF1( a ) => { val f = b( a ); xs => math.atan( f( xs ) ) }
      case LogF1( a ) => { val f = b( a ); xs => math.log( f( xs ) ) }
      case ExpF1( a ) => { val f = b( a ); xs => math.exp( f( xs ) ) }
      case Abs( a ) => { val f = b( a ); xs => math.abs( f( xs ) ) }
      case other => {
        // Fall back on the symbolic engine with the variables bound to numbers
        val names = index.toSeq
        xs => {
          val local = new Environment( Some( env ) )
          names.foreach { case ( n, i ) => local.set( n, Number( xs( i ) ) ) }
          try other.visit( Some( local ) ).eval() match {
            case Number( v ) => v
            case _ => Double.NaN
          } catch { case _: Throwable => Double.NaN }
        }
      }
    }
  }

  // Evaluate an expression with no free variables to a double (NaN if impossible)
  def number( e: Expr, env: Environment ): Double = apply( e, Nil, env )( Array() )
}
