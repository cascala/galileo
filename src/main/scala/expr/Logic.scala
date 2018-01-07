package galileo.logic

import galileo.environment.Environment
import galileo.expr._

case class Bool( boolean:Boolean ) extends Expr{
	def &(that:Bool) = Bool( this.boolean & that.boolean )
	def &(that:Number) = Bool( this.boolean & ( that.value != 0.0 ) )

	def &&(that:Bool) = Bool( this.boolean && that.boolean )
	def &&(that:Number) = Bool( this.boolean && ( that.value != 0.0 ) )

	def |(that:Bool) = Bool( this.boolean | that.boolean )
	def |(that:Number) = Bool( this.boolean | ( that.value != 0.0 ) )

	def ||(that:Bool) = Bool( this.boolean || that.boolean )
	def ||(that:Number) = Bool( this.boolean || ( that.value != 0.0 ) )

	// xor
	def ^(that:Bool) = Bool( this.boolean ^ that.boolean )
	def ^(that:Number) = Bool( this.boolean ^ (that.value != 0.0 ) )

	def unary_! = Bool( !this.boolean )
	override def toString = boolean.toString()
	def info(env:Option[Environment]=None) = "Bool(" + toString() + ")"
	def variables:List[Variable] = List()
}

trait Bool1 {
	val e:Expr
	def variables:List[Variable] = e.variables
}

trait Bool2 {
	val e:Expr
	val f:Expr
	def variables:List[Variable] = e.variables ++ f.variables
}

case class BoolAnd( e:Expr, f:Expr ) extends Expr with Bool2 {
	def info(env:Option[Environment]=None) = this.toString()
}

case class BoolOr( e:Expr, f:Expr ) extends Expr with Bool2 {
	def info(env:Option[Environment]=None) = this.toString()
}

case class BoolAndSc( e:Expr, f:Expr ) extends Expr with Bool2 {
	def info(env:Option[Environment]=None) = this.toString()
}

case class BoolOrSc( e:Expr, f:Expr ) extends Expr with Bool2 {
	def info(env:Option[Environment]=None) = this.toString()
}

case class BoolXor( e:Expr, f:Expr ) extends Expr with Bool2 {
	def info(env:Option[Environment]=None) = this.toString()
}

case class BoolNot( e:Expr ) extends Expr with Bool1 {
	def info(env:Option[Environment]=None) = this.toString()
}
