package galileo.expr

import galileo.complex.Complex
import galileo.environment.Environment
import galileo.logic.Bool

// 4.3, 3 or 1E6
case class Number(value: Double) extends Expr {
	override def toString() = value.toString
	override def factorToString() = if( value < 0 ) "(" + value.toString() + ")" else value.toString()
	override def toStringWithSign() = if( value < 0 ) value.toString else "+" + value.toString()
	override val doubleValue = value
	def *(that:Number) = Number( this.value * that.value )
	def *(that:Complex) = Complex( Product( this, that.real ), Product( this, that.imag ) )
	def +(that:Number) = Number( this.value + that.value )
	def +(that:Complex) = Complex( Sum( this, that.real ), that.imag )

	def &(that:Number) = Bool( ( this.value != 0.0 ) & ( that.value != 0.0 ) )
	def &(that:Bool) = Bool( ( this.value != 0.0 ) & that.boolean )

	def &&(that:Number) = Bool( ( this.value != 0.0 ) && ( that.value != 0.0 ) )
	def &&(that:Bool) = Bool( ( this.value != 0.0 ) && that.boolean )

	def |(that:Number) = Bool( ( this.value != 0.0 ) | ( that.value != 0.0 ) )
	def |(that:Bool) = Bool( ( this.value != 0.0 ) | that.boolean )

	def ||(that:Number) = Bool( ( this.value != 0.0 ) || ( that.value != 0.0 ) )
	def ||(that:Bool) = Bool( ( this.value != 0.0 ) || that.boolean )

	// xor
	def ^(that:Bool) = Bool( ( this.value != 0.0 ) ^ that.boolean )
	def ^(that:Number) = Bool( ( this.value != 0.0 ) ^ ( that.value != 0.0 ) )

	def unary_!() = Bool( !( this.value != 0.0 ) )
	override def unary_-() = Number( -value )

	override def >(that:Double) = this.value > that
	def info(env:Option[Environment]=None):String = "Number(" + toString() + ")"

	override def possibleFactors:List[Expr] = this.value match {
		case 0 => List()
		case 1 => List()
		case -1 => List()
		case _ => List( this )
	}

	override def extractFactor(e:Expr):Option[Expr] = { 
		//println( "Number(" + this.value + ").extractFactor(" + e + ")" )
		e match {

		case Number( 1 ) => None //Some( this ) creates a cycle
		case b if ( b == this ) => Some( Number( 1 ) )
		case b if ( b == -this ) => Some( Number( -1 ) )
		//case Number( -1 ) => Some( Number( -1 * this.value ) )
		case Number( n ) if ( Math.abs(value)>Math.abs(n) && value%n == 0 ) => Some( Number( value / n ) )
		case Number( n ) if ( n > 0 && value > n && value%n == 0 ) => Some( Number( value / n ) )
		case Number( n ) if ( n < 0 && value < n && value%n == 0 ) => Some( Number( value / n ) )
		case _ => None
	} }
}

case class NumberI( value:Int ) extends Expr {
	override def toString() = value.toString
	override def factorToString() = if( value < 0 ) "(" + value.toString() + ")" else value.toString()
	override def toStringWithSign() = if( value < 0 ) value.toString else "+" + value.toString()
	override val doubleValue = value.toDouble
	override def info(env:Option[Environment]=None):String = "NumberI(" + toString() + ")"

	def *(that:Expr):Expr = Product( this, that ).visit()

	override def possibleFactors:List[Expr] = this.value match {
		case 1 => List()
		case -1 => List()
		case _ => List( this )
	}
}
