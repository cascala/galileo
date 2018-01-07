// logarithmic functions, exp etc.
package galileo.expr

import galileo.constants.ConstantE
import galileo.environment.Environment

case class LogF1( e:Expr ) extends FunF1{
	override def toString() = "log(" + e.toString() + ")"
	def info(env:Option[Environment]=None):String = "LogF1(" + e + ")"
	override def visit( env:Option[Environment]=None):Expr = e.visit( env ) match {
		case ConstantE() => Number( 1 )
		case Power( ConstantE(), e:Expr ) => e
		case Power( a:Expr, b:Expr ) => Product( b, LogF1( a ) ).visit()
		case Number( n ) => Number( math.log( n ) )
		case f:Expr => LogF1( f )
	}
}

case class ExpF1( e:Expr ) extends FunF1 {
	override def toString() = "exp(" + e.toString() + ")"
	def info(env:Option[Environment]=None):String = "ExpF1(" + e + ")"
	override def visit( env:Option[Environment]=None):Expr = e.visit( env ) match {
		case Number( n ) => Number( math.exp( n ) )
		case f:Expr => ExpF1( f )
	}
}
