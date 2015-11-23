import org.scalatest._

import util.Random //. Random

import galileo.expr._
import galileo.trigonometry.{SinF1}
//import galileo.linalg.EyeMatrix
import galileo.tensor._

class ChristoffelTest extends FunSuite {
	import TensorIndexKind._

	val n0 = Number( 0 )
	val n1 = Number( 1 )
	val g_ab = Metric(
		List( "u", "v", "x", "y" ).map( x => Variable( x ) ),
		List( // metric
			n0,n1,n0,n0,
			n1,n0,n0,n0,
			n0,n0, Product( Number( -1 ), Power( Sum( n1, Product( Number( -1 ), Variable( "u" ) ) ), Number( 2 ) ) ), n0,
			n0,n0,n0, Product( Number( -1 ), Power( Sum( n1, Variable( "u" ) ), Number( 2 ) ) )
		), 
		Lower
	)

	test( "001") {
		val G_abc = ChristoffelFirst( g_ab )
		//info( "G_abc: " + G_abc )
		val G_uyy = G_abc.valueAt(0,3,3) // u, y, y
		val G_yyu = G_abc.valueAt(3,3,0) // y, y, u
		assert( G_uyy == Product( Number( -1 ), Sum( Variable( "u"), n1 ) ) )
		assert( G_yyu == Sum( Variable( "u"), n1 ) )
	}

	val m = Metric.twoSphere( Variable( "a" ) )
	/*
	Metric(
		List( Variable( "theta" ), Variable( "phi" ) ),
		List( // metric
			Power( Variable( "a" ), Number( 2 ) ), Number( 0 ),
			Number( 0 ), Product( Power( Variable( "a" ), Number( 2 ) ), Power( SinF1( Variable( "theta") ), Number( 2 ) ) )
		),
		Lower 
	)
*/

	test( "002") {
		val G_abc = ChristoffelFirst( m )
		val Ga_bc = ChristoffelSecond( m )
		
		assert( G_abc.valueAt(0,1,1).simplify.toString == "a^2.0*cos(theta)*sin(theta)" ) 
		assert( G_abc.valueAt(1,0,0) == G_abc.valueAt(0,1,0) ) // Symmetry
		assert( G_abc.valueAt(1,1,0).simplify.toString == "(-1.0)*a^2.0*cos(theta)*sin(theta)") // OK [10,1]

		assert( Ga_bc.valueAt(0,1,1).simplify.toString == "(-1.0)*cos(theta)*sin(theta)" ) // Christoffel^theta_phi phi
		assert( Ga_bc.valueAt(1,1,0).simplify.toString == "cos(theta)/sin(theta)" ) // Christoffel^phi_phit theta
		assert( Ga_bc.valueAt(1,1,0) == Ga_bc.valueAt(1,0,1) ) // Christoffel^phi_phit theta
	}

	// Kahn-Penrose
	val kp = Metric(
		List( "u", "v", "x", "y" ).map( x => Variable( x ) ),
		List( 
			n0, Number( 1 ), n0, n0,
			Number( 1 ), n0, n0, n0,
			n0, n0, Product( Number( -1 ), Power( Diff( Number( 1 ), Variable( "u" ) ), Number( 2 ) ) ), n0,
			n0, n0, n0, Product( Number( -1 ), Power( Sum( Number( 1 ), Variable( "u" ) ), Number( 2 ) ) )
		),
		Lower
	)
	test( "Kahn-Penrose" ) {
		val cf = ChristoffelFirst( kp )
		assert( cf.valueAt( 2,2,0 ).simplify.toString == "(-1.0)*((-1.0)*u+1.0)" /*"-1+u"*/ ) // xxu
		assert( cf.valueAt( 0,2,2 ).toString == "(-1.0)*u+1.0" /*"1-u"*/ ) // uxx
		assert( cf.valueAt( 0,3,3 ).toString == "(-1.0)*(u+1.0)" /*"-1-u"*/ ) // uyy
		assert( cf.valueAt( 3,3,0 ).toString == "u+1.0" /*"1+u"*/ ) // yyu
	}
}
