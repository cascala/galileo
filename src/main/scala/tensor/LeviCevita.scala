package galileo.tensor

import galileo.expr.{Expr,Fraction,Number,Product,Sum}

// Levi-Cevita Tensor
object LeviCevita {
	def apply(indices:TensorIndex*) = { 
		def lcValue( l:List[Int] ) = l match {
			
			case _ => Number( 1 ) // todo: fix me

		}

		val rv = Tensor( indices.toList, List[Expr]() )
		val allAddresses = ( 0 until rv.totalSize )
		Tensor( indices.toList, allAddresses.map( address => rv.location( address ) ).map( lcValue ).toList )
	}
}
