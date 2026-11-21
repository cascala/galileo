package galileo.geometry

import galileo.expr._
import galileo.tensor._

import TensorIndexKind._

// Curvature of a metric, simplifying every intermediate result.
// Mathematically the same as galileo.tensor.{ChristoffelSecond, RiemannSecond, RicciTensor, ...},
// but much faster and with readable results, because derivatives are taken of
// simplified Christoffel symbols rather than of the raw expressions.
class Curvature( val metric: Metric ) {
  val n = metric.dimension
  val xs = metric.variables
  private def zero( e: Expr ) = e match { case Number( v ) => v == 0; case _ => false }
  private def sum( ts: Seq[ Expr ] ): Expr = ts.filterNot( zero ) match {
    case Seq() => Number( 0 )
    case Seq( one ) => Tidy( one.visit() )
    case many => Tidy( Sum( many: _* ).visit() )
  }
  private def prod( a: Expr, b: Expr ): Expr = if ( zero( a ) || zero( b ) ) Number( 0 ) else Product( a, b )
  private def d( e: Expr, i: Int ): Expr = if ( zero( e ) ) Number( 0 ) else Tidy( Derivative( e, xs( i ) ).visit() )

  lazy val gl: Array[ Array[ Expr ] ] = Array.tabulate( n, n )( ( a, b ) => Tidy( metric.toLower.valueAt( a, b ) ) )
  lazy val gu: Array[ Array[ Expr ] ] = Array.tabulate( n, n )( ( a, b ) => Tidy( metric.toUpper.valueAt( a, b ) ) )

  // Gamma_abc = 1/2 (g_ab,c + g_ac,b - g_bc,a)   (first index lowered)
  lazy val christoffelFirst: Array[ Array[ Array[ Expr ] ] ] = {
    val dg = Array.tabulate( n, n, n )( ( a, b, c ) => d( gl( a )( b ), c ) )
    Array.tabulate( n, n, n )( ( a, b, c ) => sum( Seq( dg( a )( b )( c ), dg( a )( c )( b ), Product( Number( -1 ), dg( b )( c )( a ) ) ).map( t => Product( Number( 0.5 ), t ) ) ) )
  }

  // Gamma^a_bc = g^ad Gamma_dbc
  lazy val christoffel: Array[ Array[ Array[ Expr ] ] ] =
    Array.tabulate( n, n, n )( ( a, b, c ) => if ( c < b ) Number( 0 ) else sum( ( 0 until n ).map( k => prod( gu( a )( k ), christoffelFirst( k )( b )( c ) ) ) ) )
  def gamma( a: Int, b: Int, c: Int ): Expr = if ( c < b ) christoffel( a )( c )( b ) else christoffel( a )( b )( c )

  // R^a_bcd = d_c Gamma^a_bd - d_d Gamma^a_bc + Gamma^a_ce Gamma^e_bd - Gamma^a_de Gamma^e_bc
  lazy val riemann: Array[ Array[ Array[ Array[ Expr ] ] ] ] = {
    val r = Array.fill[ Expr ]( n, n, n, n )( Number( 0 ) )
    for ( a <- 0 until n; b <- 0 until n; c <- 0 until n; dd <- c + 1 until n ) {
      val v = sum( Seq( d( gamma( a, b, dd ), c ), Product( Number( -1 ), d( gamma( a, b, c ), dd ) ) ) ++
        ( 0 until n ).map( e => prod( gamma( a, c, e ), gamma( e, b, dd ) ) ) ++
        ( 0 until n ).map( e => Product( Number( -1 ), prod( gamma( a, dd, e ), gamma( e, b, c ) ) ) ).filterNot { case Product( _, x ) => zero( x ); case _ => false } )
      r( a )( b )( c )( dd ) = v
      r( a )( b )( dd )( c ) = Tidy( Product( Number( -1 ), v ).visit() )
    }
    r
  }

  // R_abcd = g_ae R^e_bcd
  lazy val riemannLower: Array[ Array[ Array[ Array[ Expr ] ] ] ] =
    Array.tabulate( n, n, n, n )( ( a, b, c, dd ) => sum( ( 0 until n ).map( e => prod( gl( a )( e ), riemann( e )( b )( c )( dd ) ) ) ) )

  // R_bd = R^a_bad
  lazy val ricci: Array[ Array[ Expr ] ] = Array.tabulate( n, n )( ( b, dd ) => sum( ( 0 until n ).map( a => riemann( a )( b )( a )( dd ) ) ) )

  lazy val ricciScalar: Expr = sum( for ( a <- 0 until n; b <- 0 until n ) yield prod( gu( a )( b ), ricci( a )( b ) ) )

  lazy val einstein: Array[ Array[ Expr ] ] = Array.tabulate( n, n )( ( a, b ) =>
    sum( Seq( ricci( a )( b ), Product( Number( -0.5 ), prod( gl( a )( b ), ricciScalar ) ) ) ) )

  lazy val weyl: Array[ Array[ Array[ Array[ Expr ] ] ] ] = {
    require( n > 2, "the Weyl tensor needs at least 3 dimensions" )
    val R = ricci; val g = gl; val s = ricciScalar
    Array.tabulate( n, n, n, n )( ( i, k, l, m ) => sum( Seq(
      riemannLower( i )( k )( l )( m ),
      Product( Number( 1.0 / ( n - 2 ) ), Sum( prod( R( i )( m ), g( k )( l ) ), Product( Number( -1 ), prod( R( i )( l ), g( k )( m ) ) ), prod( R( k )( l ), g( i )( m ) ), Product( Number( -1 ), prod( R( k )( m ), g( i )( l ) ) ) ) ),
      Product( Number( 1.0 / ( ( n - 1 ) * ( n - 2 ) ) ), s, Sum( prod( g( i )( l ), g( k )( m ) ), Product( Number( -1 ), prod( g( i )( m ), g( k )( l ) ) ) ) )
    ) ) )
  }

  // Kretschmann scalar R_abcd R^abcd = R^a_bcd R_a^bcd, computed as R_abcd R_efgh g^ae g^bf g^cg g^dh
  lazy val kretschmann: Expr = {
    // raise all indices of R_abcd one at a time
    var cur = riemannLower
    for ( p <- 0 until 4 ) {
      val prev = cur
      cur = Array.tabulate( n, n, n, n ) { ( a, b, c, dd ) =>
        val ix = Array( a, b, c, dd )
        sum( ( 0 until n ).map { e => val jx = ix.clone(); jx( p ) = e; prod( gu( ix( p ) )( e ), prev( jx( 0 ) )( jx( 1 ) )( jx( 2 ) )( jx( 3 ) ) ) } )
      }
    }
    sum( for ( a <- 0 until n; b <- 0 until n; c <- 0 until n; dd <- 0 until n ) yield prod( riemannLower( a )( b )( c )( dd ), cur( a )( b )( c )( dd ) ) )
  }

  def gaussian: Expr = { require( n == 2, "Gaussian curvature needs a 2D metric (a surface)" ); Tidy( Product( Number( 0.5 ), ricciScalar ).visit() ) }

  // flattened component lists, in the index order used by galileo.tensor.Tensor
  def flat2( t: Array[ Array[ Expr ] ] ) = ( for ( a <- 0 until n; b <- 0 until n ) yield t( a )( b ) ).toList
  def flat3( t: Array[ Array[ Array[ Expr ] ] ] ) = ( for ( a <- 0 until n; b <- 0 until n; c <- 0 until n ) yield t( a )( b )( c ) ).toList
  def flat4( t: Array[ Array[ Array[ Array[ Expr ] ] ] ] ) = ( for ( a <- 0 until n; b <- 0 until n; c <- 0 until n; dd <- 0 until n ) yield t( a )( b )( c )( dd ) ).toList
  lazy val christoffelFull = for ( a <- 0 until n; b <- 0 until n; c <- 0 until n ) yield gamma( a, b, c )
}

object Curvature {
  // keep the most recent results: notebooks typically ask several questions about one metric
  private val cache = new java.util.LinkedHashMap[ Metric, Curvature ]( 16, 0.75f, true ) {
    override def removeEldestEntry( e: java.util.Map.Entry[ Metric, Curvature ] ) = size() > 8
  }
  def apply( m: Metric ): Curvature = cache.synchronized {
    Option( cache.get( m ) ).getOrElse { val c = new Curvature( m ); cache.put( m, c ); c }
  }
}
