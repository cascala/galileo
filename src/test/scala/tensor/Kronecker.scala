import org.scalatest._

import galileo.expr.Number //,Product,Variable}
//import galileo.linalg.EyeMatrix
import galileo.tensor._

class KroneckerTest extends FunSuite {
	import TensorIndexKind._

	test( "Kronecker") {
		val d = Kronecker( TensorIndex( Upper, 3 ), TensorIndex( Lower, 3 ) )
		assert( d.valueAt( 0, 0 ) == Number( 1 ) )
		assert( d.valueAt( 1, 1 ) == Number( 1 ) )
		assert( d.valueAt( 2, 2 ) == Number( 1 ) )
		assert( d.valueAt( 1, 0 ) == Number( 0 ) )

		val e = Kronecker( TensorIndex( Upper, 1 ), TensorIndex( Lower, 3 ) )
		assert( d.valueAt( 0, 0 ) == Number( 1 ) )
		assert( d.valueAt( 0, 1 ) == Number( 0 ) )
		assert( d.valueAt( 0, 2 ) == Number( 0 ) )

		assert( d + d == d * Number( 2 ) )
	}
}
