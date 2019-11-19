package galileo.tensor

import galileo.environment.Environment
import galileo.expr._

import TensorIndexKind._

// G_abc, for metric g_ij, calc'ed as
// 1/2(g_bc,a+g_ca,b-gab,c)
object ChristoffelFirst{
	def apply( metric:Metric):ChristoffelFirst = {
		val g = metric.toLower.tensor
		val dimension = metric.dimension
		
		// it's a rank-3 tensor like object... (but recall, does not transform like a tensor
		// deep nested loops like this are a bit ugly, but let's see what we learn from this
		var components:List[Expr] = List()
		for( a <- 0 until dimension ) {
			val xa = metric.variables( a )
			for( b <- 0 until dimension ) {
				val xb = metric.variables( b )
				for( c <- 0 until dimension ) {
					val xc = metric.variables( c )
					components = components :+ Fraction( Sum( 
						Derivative( g.valueAt( b, c ), xa ),
						Derivative( g.valueAt( c, a ), xb ),
						Product( Number( -1 ), Derivative( g.valueAt(a,b), xc ) )
					), Number( 2 ) ).visit()	
				}
			}
		}
		ChristoffelFirst( metric.variables, components )
	}
}

// These are NOT tensors!
trait Christoffel extends Expr {
	val variables:List[Variable]
	val components:List[Expr]
	
	def info(env:Option[Environment]=None) = this.getClass.getSimpleName + "(" + variables.mkString + "," + components.mkString + ")"

	val dimension = variables.size
	def valueAt( location:Int* ):Expr = this.valueAt( location.to(List) )
	private def valueAt( location:List[Int]):Expr = {
		require( location.size == 3 )
		for( i <- 0 until 3 ) {
			if( location( i ) < 0 || location( i ) >= dimension )
				throw new IndexOutOfBoundsException( "location(" + i + ") out of bounds [0," + dimension + "]" )
		}

		components( address(location ) )
	}

	private def address(location:List[Int]):Int = {
		val offsets = List.fill(3)( dimension ).scanRight( 1 )(_*_).slice( 1, 4 /* any big number would do */)
		require( offsets.size == location.size )
		location.zip( offsets ).map( { case (l,o) => l * o } ).sum
	}

	// recall, Christoffel symbols do not convert like tensors!!!
	def tensor:Tensor
	def *(that:Tensor) = this.tensor * that
} 

// putting this in a case-class since it does not transform like a tensor
// this is one way to keep that cleanly separated/organized
case class ChristoffelFirst(variables:List[Variable],components:List[Expr]) extends Christoffel {
	def tensor = Tensor( List( Lower, Lower, Lower ), dimension, components )
}

// ChristoffelSecond^a_bc=g^ad * ChristoffelFirst_bcd
// You need to contract on the last entry in ChristoffelFirst, or equivalently permute !!!!
object ChristoffelSecond{
	def apply( metric:Metric):ChristoffelSecond = {
		// Contract on the 4th index d, not the 2nd index b !!!! g^ad C_bcd, 4th index
		ChristoffelSecond( metric.variables, ( metric.toUpper * ChristoffelFirst( metric ) ).contract( 1, 4 ).components )
	}
}

case class ChristoffelSecond(variables:List[Variable],components:List[Expr]) extends Christoffel {
	def tensor = Tensor( List( Upper, Lower, Lower ), dimension, components )
}

trait ChristoffelU extends Expr with Statement {
	def info(env:Option[Environment]=None) = this.getClass.getSimpleName + "(" + expr + ")"
	val expr:Expr
	val generator:Metric=>Christoffel
	override def visit(env:Option[Environment]=None) = expr.visit( env ) match {
		case metric:Metric => generator( metric )
		case a => this //.apply(a)
	}
}

// u for unhandled, so expr has not been visit-ed yet
case class ChristoffelFirstU(expr:Expr) extends ChristoffelU {
	val generator:Metric=>Christoffel = ChristoffelFirst.apply
}

case class ChristoffelSecondU(expr:Expr) extends ChristoffelU {
	val generator:Metric=>Christoffel = ChristoffelSecond.apply
}


