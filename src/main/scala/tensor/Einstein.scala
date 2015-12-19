package galileo.tensor

import galileo.expr.{Diff,Expr,Fraction,Number,Product}

// Einstein Tensor
object EinsteinTensor{
	def apply(metric:Metric) = RicciTensor(metric) - ( metric.toLower * RicciScalar( metric ) / Number( 2 ) )
}

// Trace of the Einstein Tensor
object EinsteinScalar{
	def apply(metric:Metric) = ( metric.toUpper * EinsteinTensor( metric ) ).contract( 1, 3 ).contract( 0, 1 )
}

// u for unhandled, so expr has not been visit-ed yet
case class EinsteinTensorU(expr:Expr) extends TensorU {
	val generator:Metric=>Tensor = EinsteinTensor.apply
}

case class EinsteinScalarU(expr:Expr) extends TensorU {
	val generator:Metric=>Tensor = EinsteinScalar.apply
}
