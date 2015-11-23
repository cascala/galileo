package galileo.tensor

import galileo.expr.{Diff,Fraction,Number,Product}

// Einstein Tensor
object EinsteinTensor{
	def apply(metric:Metric) = RicciTensor(metric) - ( metric.toLower * RicciScalar( metric ) / Number( 2 ) )
}

// Trace of the Einstein Tensor
object EinsteinScalar{
	def apply(metric:Metric) = ( metric.toUpper * EinsteinTensor( metric ) ).contract( 1, 3 ).contract( 0, 1 )
}
