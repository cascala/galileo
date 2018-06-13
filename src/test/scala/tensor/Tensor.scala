import org.scalatest._

import util.Random //. Random

import galileo.expr.{Number,Product,Variable}
import galileo.linalg.EyeMatrix
import galileo.tensor._

class TensorTest extends FunSuite {
	import TensorIndexKind._

	//test( "construction") {
	val t1 = Tensor( List( Lower, Upper ), 2, List( 1, 2, 3, 4 ).map( x => Number( x ) ) )	
	val t2 = Tensor( List( TensorIndex( Lower, 2 ), TensorIndex( Upper, 3 ) ), List( 1, 2, 3, 4, 3, 1 ).map( x => Number( x ) ) )
	//}

	test( "valueAt") {
		// t1 is t1_0^1
		assert( t1.rank.toInt == 2 )
		assert( t1.valueAt( 0, 0 ) == Number( 1 ) )
		assert( t1.valueAt( 0, 1 ) == Number( 2 ) )
		//assert( t1.totalSize == 4 )
		
		assert( t2.rank.toInt == 2 )
		assert( t2.valueAt( 0, 0 ) == Number( 1 ) )
		assert( t2.valueAt( 0, 2 ) == Number( 3 ) )
		assert( t2.valueAt( 1, 0 ) == Number( 4 ) )
		intercept[IndexOutOfBoundsException] { t2.valueAt(0,4) }
	}

	test( "valuesAtIndex") {
		assert( t1.valuesAtIndex(0,1).valueAt( 1 ) == Number( 4 ) ) // t1[1,:][1]
		assert( t1.valuesAtIndex(1,0).valueAt( 0 ) == Number( 1 ) ) // t1[:,0][0]
	}

	test( "sum" ){
		val s1 = t1 + t1
		assert( s1.rank == t1.rank )
		assert( s1.valueAt( 0, 0 ) == Number( 2 ) )
		val s2 = t1 + Tensor( List(), 17, List( Number( 5 ) ) ) // scalar
		assert( s2.rank == t1.rank )
		assert( s2.valueAt( 0, 0 ) == Number( 6 ) )
	}

	test( "product") {
        //                       (0,0,:,:)  (0,1,:,:) 
        //  1  2     1 2 3        1  2  3    2  4  6
        //	                      4  3  1    8  6  2
        //        x          = 
        //                       (1,0,:,:)  (1,1,:,:)
        //	3  4     4 3 1        6 12 18    4  8 12
        //	                     12  9  3   16 12  4
		val p1:Tensor = t1 * t2
		assert( p1.rank == t1.rank + t2.rank )
		assert( p1.valueAt( 0,1,0,1 ) == Number(  4 ) )
		assert( p1.valueAt( 1,0,1,0 ) == Number( 12 ) )

		val t3 = t1 * t2
		val p3 = t3.valuesAtIndex( 1, 0 ) * t3.valuesAtIndex(2,0)
		//info( "p3.indices:\n" + p3.indices )
	}

	test( "contract") {
		val t3 = t1 * t2 
		//info( "t3.indices:\n" + t3.indices )
		intercept[IllegalArgumentException] { t3.contract( 0,1 ) } // not upper, lower
		intercept[IllegalArgumentException] { t3.contract( 5,0 ) } // 5 does not exist, too high
		intercept[IllegalArgumentException] { t3.contract( 1,3 ) } // dimensions don't tie 
		val t4 = t3.contract( 1, 2 ) // contract on index 1, an upper index (contravariant) and 2, a lower index (covariant)
		assert( t3.rank.toInt == t4.rank.toInt + 2 )
		//info( "t4.indices:\n" + t4.indices )
	}

	// covariant 4-position, in Minkowski space
	val t = Variable( "t" )
	val x = Variable( "x" )
	val y = Variable( "y" )
	val z = Variable( "z" )
	val X_m = Tensor( List( Lower ), 4,  List( t, -x, -y, -z ) )
	// todo: Allow matrix to specify elements
	// Minkowski metric, nu^uv
	val nu = Tensor( List( Upper, Upper ), 4,  List( // metric
		Number( 1 ), Number( 0 ),  Number( 0 ),  Number( 0 ),
		Number( 0 ), Number( -1 ), Number( 0 ),  Number( 0 ),
		Number( 0 ), Number( 0 ),  Number( -1 ), Number( 0 ),
		Number( 0 ), Number( 0 ),  Number( 0 ),  Number( -1 )
	) )
	
	test( "raise" ) {
		val Xm = ( nu * X_m ).contract( 1, 2 ) // lowercase metric, nu_uv
		assert( Xm.rank.upper == X_m.rank.upper + 1 )
		assert( Xm.rank.lower == X_m.rank.lower - 1 )
		assert( Xm.valueAt( 0 ) == t )
		assert( Xm.valueAt( 1 ) == x )
		assert( Xm.valueAt( 3 ) == z )
	}

	// ElectroMagnetic tensor
	val E_x = Variable( "E_x" )
	val E_y = Variable( "E_y" )
	val E_z = Variable( "E_z" )
	val B_x = Variable( "B_x" )
	val B_y = Variable( "B_y" )
	val B_z = Variable( "B_z" )
	
	// contravariant, F^ab
	val Fab = Tensor( List( Upper,  Upper ), 4, List(
		Number( 0 ), -E_x, -E_y, -E_z, // row 0i
		E_x, Number( 0 ), -B_z, B_y, // row 1i
		E_y, B_z, Number( 0 ), -B_x, // row 2i
		E_z, -B_y, B_x, Number( 0 ) // row 3i
	) ) // after lowering will be F_ab, or after lowering first index would be Fb_a

	// this does not work yet
	test( "metricInverse" ) {
		val n_u = Tensor.metricInverse( nu )
		assert( n_u.indices.size == nu.indices.size )
		//info( "n_u\n" + n_u )
		assert( Product( n_u.toMatrix, nu.toMatrix ).visit() == EyeMatrix( nu.indices(0).dimension ).toDenseMatrix ) // should check num tol
	}
	
	test( "lower" ) {
		val n_u = Tensor( nu.indices.map( index => index.inverse ), nu.components )
		val Fa_b = (Fab*n_u).contract(1,2) // 'lower' b
		val F_ab1 = (n_u*Fa_b).contract(2,0) // lower a
		val F_ab2 = (n_u*Fa_b).contract(2,1) // you contract on either 1 or 0
		assert( F_ab1 == F_ab2 )
		assert( F_ab1.valueAt(1,0) == -E_x )
		assert( F_ab1.valueAt(0,1) == E_x )
		assert( F_ab1.valueAt(1,2) == -B_z )
		assert( F_ab1.valueAt(1,3) ==  B_y)
		assert( F_ab1.valueAt(2,3) == -B_x )
		assert( F_ab1.valueAt(3,3) == Number( 0 ) )
	}

	test( "symmetry") {
		val rng = new Random()
		val num1 = List.fill(9)(0).map( x => rng.nextInt( 500 ) )
		val ran1 = Tensor( List( Upper,Upper), 3, num1.map( x => Number( x ) ) )
		val sym1 = ( ran1 + ran1.swapIndices(0,1) ) / Number( 2 )

		val num2 = List.fill(9)(0).map( x => rng.nextInt( 500 ) )
		val ran2 = Tensor( List( Lower,Lower), 3, num2.map( x => Number( x ) ) )
		val asym2 = ( ran2 - ran2.swapIndices( 0,1) ) / Number( 2 )

		// R^ab*T_ab with R symmetric and T assymmetric leads to a zero scalar
		val prod = ( sym1 * asym2 ).contract(1,3).contract(0,1)
		assert( prod.components.size == 1 )
		assert( prod.valueAt() == Number( 0 ) )
	}
}
