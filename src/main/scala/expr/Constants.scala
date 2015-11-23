package galileo.constants

import galileo.complex._
import galileo.environment.Environment
import galileo.expr._

trait Constant extends Expr { // has dimensions - todo
	val value:Expr
	val shortName:String
	override def toString() = shortName //value.toString()
	def info(env:Option[Environment]=None) = "Constant(" + shortName + ")"
	override def eval() = value
}

case class ConstantPi() extends Constant {
	val value = Number( Math.PI ) // 3.1415926535897932384626433832795028841971693 )	
	val shortName = "pi"
}

case class ConstantE() extends Constant {
	val value = Number( Math.E ) //2.71828182845904523536028747135266249775724709369995 )
	val shortName = "e"
}

class ConstantJ extends Constant {
	val value = Complex( Number( 0 ), Number( 1 ) )
	val shortName = "j"

	override def visit( env:Option[Environment]=None):Expr = this.value
}

/*
trait Constant{
	def eval 
}


class ConstantPi() extends Constant with Expr {
	val name = "pi"
	val value = 3.1415 // todo
//	def eval
}


*/
