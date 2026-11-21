package galileo.plot

import galileo.constants._
import galileo.environment.Environment
import galileo.expr._
import galileo.trigonometry._

// Anything that can render itself as rich output (e.g. in a Jupyter notebook).
// The map goes from MIME type to content, e.g. "image/svg+xml" -> "<svg ...>"
trait Displayable {
  def mimeBundle: Map[String, String]
}

// plot( f1, f2, ..., x, from, to )
case class Plot( exprs: List[Expr], variable: Variable, from: Expr, to: Expr ) extends Expr with Statement {
  def info( env: Option[Environment] = None ) = "Plot(" + exprs.mkString( "," ) + "," + variable + "," + from + "," + to + ")"
  override def toString() = "plot(" + ( exprs ++ List( variable, from, to ) ).mkString( "," ) + ")"

  override def visit( env: Option[Environment] = None ): Expr = {
    val e = env.getOrElse( new Environment() )
    val ( a, b ) = ( Numeric.eval( from, e ), Numeric.eval( to, e ) )
    if ( a.isNaN || b.isNaN || a == b )
      return ErrorExpr( "plot: the range " + from + " .. " + to + " must be two different numbers" )
    val ( lo, hi ) = ( math.min( a, b ), math.max( a, b ) )
    val n = Plot.samples
    val xs = ( 0 to n ).map( i => lo + ( hi - lo ) * i / n )
    val series = exprs.map { expr =>
      val f = Numeric.compile( expr, variable.name, e )
      Series( expr.toString, xs.map( x => ( x, f( x ) ) ).toVector )
    }
    Figure( series, variable.name, "" )
  }
}

object Plot {
  val samples = 400

  // plot( f, x ) uses a default range, plot( f, g, x, a, b ) an explicit one
  def fromArgs( args: List[Expr] ): Expr = args.reverse match {
    case to :: from :: ( v: Variable ) :: rest if rest.nonEmpty => Plot( rest.reverse, v, from, to )
    case ( v: Variable ) :: rest if rest.nonEmpty => Plot( rest.reverse, v, Number( -10 ), Number( 10 ) )
    case _ => ErrorExpr( "usage: plot(f, x) or plot(f1, f2, ..., x, from, to)" )
  }
}

// parametric( fx, fy, t, from, to )
case class ParametricPlot( fx: Expr, fy: Expr, variable: Variable, from: Expr, to: Expr ) extends Expr with Statement {
  def info( env: Option[Environment] = None ) = "ParametricPlot(" + fx + "," + fy + "," + variable + "," + from + "," + to + ")"
  override def toString() = "parametric(" + List( fx, fy, variable, from, to ).mkString( "," ) + ")"

  override def visit( env: Option[Environment] = None ): Expr = {
    val e = env.getOrElse( new Environment() )
    val ( a, b ) = ( Numeric.eval( from, e ), Numeric.eval( to, e ) )
    if ( a.isNaN || b.isNaN || a == b )
      return ErrorExpr( "parametric: the range " + from + " .. " + to + " must be two different numbers" )
    val gx = Numeric.compile( fx, variable.name, e )
    val gy = Numeric.compile( fy, variable.name, e )
    val n = Plot.samples
    val pts = ( 0 to n ).map { i => val t = a + ( b - a ) * i / n; ( gx( t ), gy( t ) ) }.toVector
    Figure( List( Series( "(" + fx + ", " + fy + ")", pts ) ), "", "" )
  }
}

// Numerical evaluation of symbolic expressions, used for plotting
object Numeric {
  private val maxDepth = 64

  // Evaluate an expression without free variables to a double (NaN if that is impossible)
  def eval( e: Expr, env: Environment ): Double = compile( e, "", env )( 0.0 )

  // Turn expr into a function of the variable called name
  def compile( expr: Expr, name: String, env: Environment ): Double => Double = {
    val prepared = prepare( expr, name, env, 0 )
    ( x: Double ) => {
      try num( prepared, name, x, env ) catch { case _: Throwable => Double.NaN }
    }
  }

  // Substitute user-defined variables (except the plot variable) and
  // work out symbolic derivatives once, before sampling
  private def prepare( e: Expr, name: String, env: Environment, depth: Int ): Expr = {
    def p( s: Expr ) = prepare( s, name, env, depth + 1 )
    if ( depth > maxDepth ) return e
    e match {
      case Variable( n ) if n == name => e
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
      case Max( as @ _* ) => Max( as.map( p ): _* )
      case Derivative( y, v ) => p( Derivative( p( y ), v ).visit( Some( new Environment() ) ) )
      case other => other
    }
  }

  private def num( e: Expr, name: String, x: Double, env: Environment ): Double = {
    def n( s: Expr ) = num( s, name, x, env )
    e match {
      case Number( v ) => v
      case NumberI( v ) => v.toDouble
      case Variable( v ) if v == name => x
      case Variable( "pi" ) => math.Pi
      case Variable( "e" ) => math.E
      case c: Constant => c.value match {
        case Number( v ) => v
        case _ => Double.NaN
      }
      case Sum( terms @ _* ) => terms.foldLeft( 0.0 )( ( acc, t ) => acc + n( t ) )
      case Product( factors @ _* ) => factors.foldLeft( 1.0 )( ( acc, f ) => acc * n( f ) )
      case Power( a, b ) => math.pow( n( a ), n( b ) )
      case Fraction( a, b ) => n( a ) / n( b )
      case SinF1( a ) => math.sin( n( a ) )
      case CosF1( a ) => math.cos( n( a ) )
      case TanF1( a ) => math.tan( n( a ) )
      case AsinF1( a ) => math.asin( n( a ) )
      case AcosF1( a ) => math.acos( n( a ) )
      case AtanF1( a ) => math.atan( n( a ) )
      case LogF1( a ) => math.log( n( a ) )
      case ExpF1( a ) => math.exp( n( a ) )
      case Abs( a ) => math.abs( n( a ) )
      case Max( as @ _* ) => as.map( n ).max
      case other => {
        // Fall back on the symbolic engine, with the plot variable bound to x
        val local = new Environment( Some( env ) )
        if ( name.nonEmpty ) local.set( name, Number( x ) )
        other.visit( Some( local ) ).eval() match {
          case Number( v ) => v
          case r if r != other => num( r, name, x, env )
          case _ => Double.NaN
        }
      }
    }
  }
}
