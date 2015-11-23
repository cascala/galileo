import org.scalatest._

import galileo.expr.{Number,Product,Variable}
import galileo.tensor._
import galileo.trigonometry.{CosF1,SinF1}

class CoordinateTransformationTest extends FunSuite {
	import TensorIndexKind._
	// x, y, orthogonal, orthonormal
	// ds^2=dx^2+dy^2, so 
	val g_ab=Kronecker( TensorIndex( Lower, 2 ), TensorIndex( Lower, 2 ) )

	val r = Variable( "r" )
	val omega = Variable( "omega" )
	val phi = Variable( "phi")
	val x = Product( r, SinF1( omega ), CosF1( phi ) )
	val y = Product( r, SinF1( omega ), SinF1( phi ) )
	val z = Product( r, CosF1( omega ) )

	// Transformation matrix
	val Lambda = Tensor.TransformationMatrix( 
		List( x, y, z ), // old coords, unprimed
		List( r, omega, phi ) // new coords, primed
	)

	test( "test1") {
		assert( Lambda.valueAt( 0, 2 ) == CosF1( omega ) )
		assert( Lambda.valueAt( 2, 1 ) == Product( r, SinF1( omega ), CosF1( phi ) ))
		assert( Lambda.valueAt( 2, 2 ) == Number( 0 ) )
	}
}
