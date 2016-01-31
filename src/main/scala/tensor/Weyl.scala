package galileo.tensor

import galileo.expr.{Expr,Fraction,Number,Product,Sum} //{Diff,Expr,Fraction,Number,Product}

// Weyl Tensor
object WeylTensor{
	def apply(metric:Metric) = { 
		require( metric.dimension > 2 )
		var components:List[Expr] = List()
		val riemannFirst = RiemannFirst( metric )
		val ricci = RicciTensor( metric )
		val ricciScalar = RicciScalar( metric ).valueAt()
		val metricL = metric.toLower
		for( i <- 0 until metric.dimension )
			for( k <- 0 until metric.dimension )
				for( l <- 0 until metric.dimension )
					for( m <- 0 until metric.dimension )
						components = components :+ entry( riemannFirst, ricci, metricL, ricciScalar, i, k, l, m )
		Tensor( indices( metric.dimension ), components )				
	}

	import TensorIndexKind.Lower
	private def indices(dimension:Int) = List.fill(4)(TensorIndex(Lower,dimension))

	private def entry(riemannFirst:Tensor, ricci:Tensor, metric:Metric, ricciScalar:Expr, i:Int, k:Int, l:Int, m:Int ) = {
		Sum(
			riemannFirst.valueAt( i, k, l, m ),
			Fraction( 
				Product( ricci.valueAt( i, m ), metric.valueAt( k, l ) ) - Product( ricci.valueAt( i, l ), metric.valueAt( k, m ) )
				+ Product( ricci.valueAt( k, l ), metric.valueAt( i, m ) ) - Product( ricci.valueAt( k, m ), metric.valueAt( i, l ) ),
				Number( metric.dimension - 2 ) ),
			Product( ricciScalar, ( Product( metric.valueAt(i,l), metric.valueAt(k,m) ) - Product( metric.valueAt(i, m), metric.valueAt( k, l ) ) ), Fraction( Number( 1 ), Number( (metric.dimension - 2 )*(metric.dimension - 1 ) ) ) )
		)		
	} 
}

// u for unhandled, so expr has not been visit-ed yet
case class WeylTensorU(expr:Expr) extends TensorU {
	val generator:Metric=>Tensor = WeylTensor.apply
}