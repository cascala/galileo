package galileo.tensor

import galileo.expr.{Diff,Expr,Fraction,Number,Product}

// Schouten Tensor
object SchoutenTensor{
	def apply(metric:Metric) = { 
		require( metric.dimension > 2 )
		( RicciTensor(metric) - ( metric.toLower * RicciScalar( metric ) / Number( 2 * ( metric.dimension - 1 ) ) ) ) / Number( metric.dimension - 2 )
	}
}

// u for unhandled, so expr has not been visit-ed yet
case class SchoutenTensorU(expr:Expr) extends TensorU {
	val generator:Metric=>Tensor = SchoutenTensor.apply
}
