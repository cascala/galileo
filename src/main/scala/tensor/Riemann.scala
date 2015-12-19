package galileo.tensor

import galileo.expr._

trait Riemann{
	def entry(metric:Metric, cf:ChristoffelFirst, cs:ChristoffelSecond, a:Int, b:Int, c:Int, d:Int):Expr
	def indices(dimension:Int):List[TensorIndex]

	def apply(metric:Metric):Tensor = {
		
		var components:List[Expr] = List()
		val metricL = metric.toLower
		val cf = ChristoffelFirst( metric )
		val cs = ChristoffelSecond( metric ) 
		for( a <- 0 until metric.dimension) {
			for( b <- 0 until metric.dimension) {
				for( c <- 0 until metric.dimension) {
					for( d <- 0 until metric.dimension ) {
						components = components :+ entry( metricL, cf, cs, a, b, c, d)	
					}
				}
			}
		}
		Tensor( indices(metric.dimension), components )
	}
}

// R^a_bcd
object RiemannSecond extends Riemann {
	import TensorIndexKind.{Lower,Upper}
	def indices(dimension:Int) = List(Upper,Lower,Lower,Lower).map( kind => TensorIndex(kind,dimension) )
	def entry( metric:Metric, cf:ChristoffelFirst, cs:ChristoffelSecond, a:Int, b:Int, c:Int, d:Int ):Expr = {
		var rv:Expr = Diff(
			Derivative( cs.valueAt(a,b,d), metric.variables(c) ),
			Derivative( cs.valueAt(a,b,c), metric.variables(d) )
		)
		for( e <- 0 until metric.dimension)
			rv = Sum( rv, Product( cs.valueAt(e,b,d), cs.valueAt(a,e,c) ) )
		for( e <- 0 until metric.dimension)
			rv = Sum( rv, Product( Number( -1 ), cs.valueAt(e,b,c), cs.valueAt(a,e,d) ) )
		rv.visit()
	}
}


// R_abcd
object RiemannFirst extends Riemann {
	import TensorIndexKind.Lower
	def indices(dimension:Int) = List.fill(4)(TensorIndex(Lower,dimension))
	def entry( metric:Metric, cf:ChristoffelFirst, cs:ChristoffelSecond, a:Int, b:Int, c:Int, d:Int ):Expr = {
		var rv:Expr  = Fraction(
			Diff( 
				Sum( 	
					Derivative(Derivative(metric.valueAt(a,d),metric.variables(b)),metric.variables(c)),
					Derivative(Derivative(metric.valueAt(b,c),metric.variables(a)),metric.variables(d))
				),
				Sum(
					Derivative(Derivative(metric.valueAt(a,c),metric.variables(b)),metric.variables(d)),
					Derivative(Derivative(metric.valueAt(b,d),metric.variables(a)),metric.variables(c))
				)
			),
			Number(2)
		)
		for( e <- 0 until metric.dimension)
			rv = Sum( rv, Product( cf.valueAt(a,d,e), cs.valueAt(e,b,c) ) )
		for( e <- 0 until metric.dimension)
			rv = Sum( rv, Product( Number( -1 ), cf.valueAt(a,c,e), cs.valueAt(e,b,d) ) )
				
		rv.visit()
	}
}

case class RiemannFirstU(expr:Expr) extends TensorU {
	val generator:Metric=>Tensor = RiemannFirst.apply
}

case class RiemannSecondU(expr:Expr) extends TensorU {
	val generator:Metric=>Tensor = RiemannSecond.apply
}
