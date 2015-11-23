import org.scalatest._

import galileo.expr.Variable
import galileo.tensor.{TensorIndex,TensorIndexKind}

class TensorIndexTest extends FunSuite {
	import TensorIndexKind._
	test( "001") {
		val i = TensorIndex( Upper, 3 )
		val j = TensorIndex( Lower, 2 )

		assert( i.kind == Upper )
		assert( i.inverse.kind == Lower )	
	}
}
