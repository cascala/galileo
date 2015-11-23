package galileo.tensor

import galileo.expr.{Derivative,Expr}

import TensorIndexKind._

// Todo: Offer a CovariantDerivative version that takes a Expr argument, not an index

// also known as the semi-colon (;) derivative
// answer in pieces:
// * partial derivative
// * add Christoffel term for each contravariant index
// * sub Christoffel term for each covariant index
object CovariantDerivative {
	def apply( metric:Metric, tensor:Tensor):Tensor = {
		val indices = List( TensorIndex( Lower, metric.dimension ) ) ++ tensor.indices
		var components:List[Expr] = List()
		val cs = ChristoffelSecond( metric )
		for( i <- 0 until metric.variables.size )
		{
			val variable = metric.variables( i )
			var rv = CommaDerivative( tensor, variable )
			for( l <- 0 until tensor.indices.size ) {
				tensor.indices(l).kind match {
					case Upper => rv = rv + ( cs * tensor ).contract(l+3,2).valuesAtIndex(1,i) // contract(upper,lower) ULL*
					case Lower => rv = rv - ( cs * tensor ).contract(0,l+3).valuesAtIndex(1,i) 
				}
			}
			components = components ++ rv.components
		}
		Tensor( indices, components )
	}
}


/*
\Nabla_index Tensor
*/
/*
case class CovariantDerivative(
	metric:Metric,
	tensor:Tensor,
	indec:Int
)
*/

// simple partial derivative
object CommaDerivative {
	def apply(tensor:Tensor, c:Expr):Tensor = {
		Tensor( tensor.indices, tensor.components.map( component => Derivative( component, c ).visit() ) )
	}

	def apply( metric:Metric, tensor:Tensor):Tensor = {
		val indices = List( TensorIndex( Lower, metric.dimension ) ) ++ tensor.indices
		var components:List[Expr] = List()
		for( i <- 0 until metric.variables.size )
		{
			val variable = metric.variables( i )
			var rv = CommaDerivative( tensor, variable )
			components = components ++ rv.components
		}
		Tensor( indices, components )
	}
}

