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
}

case class BoolAnd( e:Expr, f:Expr ) extends Expr {
	def info(env:Option[Environment]=None) = this.toString()
}

case class BoolOr( e:Expr, f:Expr ) extends Expr {
	def info(env:Option[Environment]=None) = this.toString()
}

case class BoolAndSc( e:Expr, f:Expr ) extends Expr {
	def info(env:Option[Environment]=None) = this.toString()
}

case class BoolOrSc( e:Expr, f:Expr ) extends Expr {
	def info(env:Option[Environment]=None) = this.toString()
}

case class BoolXor( e:Expr, f:Expr ) extends Expr {
	def info(env:Option[Environment]=None) = this.toString()
}

case class BoolNot( e:Expr ) extends Expr {
	def info(env:Option[Environment]=None) = this.toString()
}
