package galileo.tensor

import galileo.expr.{Expr,Fraction,Number,Product,Sum}

// Kronecker delta
object Kronecker {
	def apply(indices:TensorIndex*) = { 
		val sz = indices.map( index => index.dimension ).foldLeft( Int.MaxValue )(math.min(_,_))
		DiagTensor(indices.toList, List.fill( sz )( Number( 1 ) ) )
	}
}

// efficient Diagonal element only tensor
case class DiagTensor( indices:List[TensorIndex], elements:List[Expr] ) extends TensorTrait {
	private lazy val rankInt = indices.size.toInt
	// Move to trait?
	private def address( location:List[Int] ):Int = {
		val offsets = indices.map( index => index.dimension ).scanRight( 1 )(_*_).slice( 1, rankInt+1 /* any big number would do */)
		require( offsets.size == location.size )
		location.zip( offsets ).map( { case (l,o) => l * o } ).sum
	}

	//def +(that:TensorTrait):TensorTrait = {}
	def components:List[Expr] = {
		val locations:List[List[Int]] = List()
		val sz = indices.map( index => index.dimension ).foldLeft(1)(_*_)
		var rv:List[Expr] = List.fill( sz )( Number( 0 ) )
		val minDim = indices.map( index => index.dimension ).foldLeft(0)(math.min(_,_))

		for( i <- 0 to minDim ) {
			val location = List.fill(indices.size)( i )
			rv = rv.updated( address( location ), Number( 1 ) )
		}
		rv
	}

	def valueAt( location:Int*) = {
		val dims = indices.map( index => index.dimension )
		// check dimensionality
		location.toList.zip( dims ).foreach( { case (x,y) => require ( x < y ) } )
		val head = location.head
		location.toList.foldRight( true )( (_1,_2) => _2 && ( _1 == head ) ) match {
			case true => elements( head )
			case false => Number( 0 )
		}
	}

	def +(that:DiagTensor) = {
		require( this.indices == that.indices )
		DiagTensor( this.indices, this.elements.zip( that.elements ).map( { case (_1,_2) => Sum( _1,_2 ).visit() } ) ) 
	}

	def *(that:Expr) = 
		DiagTensor( this.indices, this.elements.map( element => Product( element, that ).visit() ) )

	def /(that:Expr) = 
		DiagTensor( this.indices, this.elements.map( element => Fraction( element, that ).visit() ) )

	def swapIndices(left:Int,right:Int) = {
		require( 0 <= left && left < this.rankInt )
		require( 0 <= right && right < this.rankInt )
		val swappedIndices = this.indices.updated(left,this.indices(right)).updated(right,this.indices(left))
		val swappedElements = this.elements.updated( left,elements( right)).updated(right,elements(left))
		DiagTensor( swappedIndices, swappedElements)
	}

}
