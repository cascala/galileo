package galileo.tensor

import galileo.environment.Environment
import galileo.expr._
import galileo.linalg.{DenseMatrix,Matrix}
import galileo.selectable.Selectable

object TensorIndexKind extends Enumeration {
	type TensorIndexKind = Value
	val Upper, Lower = Value
}

import TensorIndexKind._

case class TensorIndex( kind:TensorIndexKind, dimension:Int) {
	def inverse = kind match {
		case Upper => TensorIndex( Lower, dimension )
		case Lower => TensorIndex( Upper, dimension )
	}
	//def variables = 
}

// to be renamed Tensor after Tensor is renamed DenseTensor
trait TensorTrait {
	def variables:List[Variable] =  {
		throw new IllegalArgumentException( "variables should not be called on a Tensor" )
		List()
	}
}

object Tensor {
	// All indices are assumed to be of the same dimension
	def apply(kinds:List[TensorIndexKind],dimension:Int,components:List[Expr]):Tensor = Tensor(
		kinds.map( kind => TensorIndex( kind, dimension ) ),
		components
	)

	def metricInverse(metric:Tensor):Tensor = {
		val indices = metric.indices.map( index => index.inverse )
		val m = metric.toMatrix.inverse.toDenseMatrix.rows.flatten
		Tensor( indices, m )
	}

	def transformationMatrix( unprimed:List[Expr], primed:List[Expr]):Tensor = { 
		require( primed.size == unprimed.size )
		//var jacobian:List[Expr] = List()
		var jacobian = primed.map( primedE => unprimed.map( unprimedE => Derivative( unprimedE, primedE ).visit() ) ).flatten

		Tensor(
			List( Upper, Lower ),
			primed.size,
			jacobian
		)
	}
}

case class TensorRank(upper:Int,lower:Int){
	def toInt:Int = upper + lower
	def +(that:TensorRank) = TensorRank( this.upper + that.upper, this.lower + that.lower )
}

// Should really extend TensorTrait .. this should be called DenseTensor
case class Tensor( indices:List[TensorIndex], components:List[Expr]) extends Expr with Selectable {
	def rank:TensorRank = TensorRank( indices.filter( x => x.kind == Upper ).size, indices.filter( x => x.kind == Lower ).size )
	lazy val rankInt = rank.toInt
	def info(env:Option[Environment]=None) = "Tensor(" + indices + "," + components + ")"
	def toMatrix:Matrix = {
		require( indices.size == 2 )
		DenseMatrix( components.grouped(indices(1).dimension ).to(List) )
	}

	def valueAt( location:Int* ):Expr = this.valueAt( location.to(List) )
	private def valueAt( location:List[Int]):Expr = {
		require( location.size == rankInt )
		for( i <- 0 until rankInt ) {
			if( location( i ) < 0 || location( i ) >= indices(i).dimension )
				throw new IndexOutOfBoundsException( "location(" + i + ") out of bounds [0," + indices(i).dimension + "]" )
		}

		components( address(location ) )
	}

	def swapIndices( left:Int,right:Int) = { 
		require( 0 <= left )
		require( left < this.rankInt )
		require( 0 <= right )
		require( right < this.rankInt )

		val swappedIndices = this.indices.updated(left,this.indices(right)).updated(right,this.indices(left))
		//val allAddresses = ( 0 until this.to(List)alSize )
		var swappedComponents:List[Expr] = List()
		for( i <- 0 until this.totalSize ) {
			var loc = this.location( i )
			loc = loc.updated( left, loc( right ) ).updated( right, loc( left ) )
			val comp = this.components( this.address( loc ) )
			swappedComponents = swappedComponents :+ comp
		}
		Tensor( swappedIndices, swappedComponents )
	}
	// Reduced rank tensor, extract with a specific value of index
	def valuesAtIndex(index:Int,value:Int):Tensor = {
		require( index < this.rankInt )
		require( value >= 0 )
		require( value < indices(index).dimension )
		Tensor( indices.patch( index, Nil, 1 ), locations( index, value ).map( location => this.valueAt( location ) ).to(List) )
	}

	// all locations for all indices associated with index set to value
	// e.g. index = 1, value = 2: Find all locations (:,2,:,:....)
	private def locations( index:Int,value:Int) = {
		val allAddresses = ( 0 until this.totalSize )
		allAddresses.map( address => this.location( address ) ).filter( location => location( index ) == value )
	}

	def location( address:Int ):List[Int] = {
		val offsets = indices.map( index => index.dimension ).scanRight( 1 )(_*_).slice( 1, rankInt+1 /* any big number would do */)
		var rem = address
		offsets.foldLeft(List[Int]()){ case ( l,e ) => { val q = rem / e; rem -= q * e; l :+ q } }
	}

	def totalSize = indices.map( index => index.dimension ).foldLeft(1)(_*_)

	// Let's say we have the following dimensions:
	// 4, 3, 5
	// location     address
	// 0,0,0 	->   0
	// 0,0,1    ->   1
	// 0,0,4    ->   4
	// 0,1,0,   ->   5
	// 0,1,4    ->   9
	// 0,2,0    ->  10
	// 0,2,4    ->  14
	// 1,0,0    ->  15
	// 2,0,0    ->  30
	// 3,0,0    ->  45
	// 3,2,4    ->  59 (last address)
	// offsets, in this case would be 15,5,1
	private def address( location:List[Int] ):Int = {
		val offsets = indices.map( index => index.dimension ).scanRight( 1 )(_*_).slice( 1, rankInt+1 /* any big number would do */)
		require( offsets.size == location.size )
		location.zip( offsets ).map( { case (l,o) => l * o } ).sum
	}

	//def *(that:Expr) = Tensor( this.indices, this.components.map( component => Product( component, that ).visit() ) )
	// element wise division
	def /(that:Expr) = Tensor( this.indices, this.components.map( component => Fraction( component, that ).visit() ) )

	// Use this to reduce rank,
	// E.g Aj_l <- contract Ajk_kl on k, upperIndex=1, lowerIndex=2 (indices go like upper j=0, upper k=1, lower k=2, lower l=3)
	// Mechanically this means
	// Aj_l = Aj0_0l+Aj1_1l+Aj2_2l...
	def contract( upperIndex:Int, lowerIndex:Int ):Tensor = {
		require( this.rank.upper > 0 )
		require( this.rank.lower > 0 )
		require( upperIndex < this.rankInt )
		require( lowerIndex < this.rankInt )
		require( this.indices( upperIndex ).kind == Upper )
		require( this.indices( lowerIndex ).kind == Lower )
		require( this.indices( upperIndex ).dimension == this.indices( lowerIndex ).dimension )

		// remove the lower and upper indices
		val before = this.indices.size
		val productIndices = this.indices.zipWithIndex.filter( { case (_,_2) => _2 != upperIndex && _2 != lowerIndex } ).map( _._1 )
		val after = productIndices.size
		require( before == after + 2 )
		val productSize = productIndices.map( index => index.dimension ).foldLeft(1)(_*_)
		var rv:Tensor = Tensor( productIndices,  List.fill( productSize )( Number( 0 ) ) )
		
		val n = this.indices( upperIndex ).dimension
		for( i <- 0 until n ) { // do we need to change order if upperIndex > lowerIndex?
			if( upperIndex < lowerIndex )
				rv = rv + ( this.valuesAtIndex( upperIndex, i ).valuesAtIndex( lowerIndex - 1, i ) ) //.visit()
			else
				rv = rv + ( this.valuesAtIndex( lowerIndex, i ).valuesAtIndex( upperIndex - 1, i ) ) //.visit()
		}
		rv
	}

	// Tensor + Expr
	override def +(that:Expr) = that match {
		case t:Tensor => this tensorPlus t
		case e:Expr => Tensor( this.indices, this.components.map( component => Sum( component, that ).visit() ) )
	}
	def +(that:Tensor):Tensor = this tensorPlus that 

	// Tensor + Tensor
	private def tensorPlus(that:Tensor):Tensor = that.indices match {
		case Nil => { 
			require( that.components.size == 1 )
			Tensor( this.indices, components.map( component => Sum( component, that.components( 0 ) ).visit() ) )
		}
		case l:List[TensorIndex] => {
			require( this.indices == l, "l:\n" + l + "\nthis.indices\n" + this.indices )
			Tensor( this.indices, this.components.zip( that.components ).map( { case ( _1:Expr,_2:Expr) => Sum(_1,_2).visit() } ) )
		}
	}

	// Tensor - Tensor
	def -(that:Tensor) = that.indices match {
		case Nil => { 
			require( that.components.size == 1 )
			Tensor( this.indices, components.map( component => Sum( component, Product( Number( -1 ), that.components( 0 ) ) ).visit() ) )
		}
		case l:List[TensorIndex] => {
			require( this.indices == l, "l:\n" + l + "\nthis.indices\n" + this.indices )
			Tensor( this.indices, this.components.zip( that.components ).map( { case ( _1:Expr,_2:Expr) => Sum(_1,Product(Number(-1),_2)).visit() } ) )
		}
	}

	// Tensor * Expr
	def *(that:Expr):Tensor = that match {
		case t:Tensor => this tensorTimes t
		case e:Expr => Tensor( this.indices, this.components.map( component => Product( component, that ).visit() ) )
	}

	// Tensor * Tensor
	def tensorTimes(that:Tensor) = Tensor( 
		this.indices ++ that.indices,
		this.components.map( thiscomponent => that.components.map( thatcomponent => Product( thiscomponent, thatcomponent ).visit() ) ).flatten
	)

	override def visit(env:Option[Environment]=None) = Tensor( indices, components.map( component => component.visit( env ) ) )
	override def expand = Tensor( indices, components.map( component => component.expand ) )
	override def simplify = Tensor( indices, components.map( component => component.simplify ) )
	override def factor = Tensor( indices, components.map( component => component.factor ) )

	// todo - clearly :)
	def select(indices:List[Expr]):Expr = {
		Number( 3 )
	}

	def variables:List[Variable] = components.flatMap( component => component.variables )
}

trait TensorU extends Expr with Statement {
	def info(env:Option[Environment]=None) = this.getClass.getSimpleName + "(" + expr + ")"
	val expr:Expr
	val generator:Metric=>Tensor
	override def visit(env:Option[Environment]=None) = expr.visit( env ) match {
		case metric:Metric => generator( metric )
		case a => this //.apply(a)
	}
}
