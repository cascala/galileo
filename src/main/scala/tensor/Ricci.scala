package galileo.tensor

// This is NOT the definition followed by Daniel Zwillinger in 'CRC Standard Mathematical Tables and Formulae'
// It does align with:
// * http://www.physicspages.com/2014/10/21/ricci-tensor-and-curvature-scalar-for-a-sphere/
// * Relativity Demistified
object RicciTensor {
	def apply(metric:Metric) = RiemannSecond(metric).contract(0,2).visit()
}

object RicciScalar {
	def apply(metric:Metric) = (metric.toUpper * RicciTensor( metric ) ).contract( 0,2).contract(0,1).visit()
}
