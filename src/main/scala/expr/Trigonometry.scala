package galileo.trigonometry

import galileo.constants._
import galileo.environment.Environment
import galileo.expr._
import galileo.logic.Bool

trait TrigF1 extends FunF1 {
}

trait AtrigF1 extends FunF1 {
}

case class SinF1( e:Expr ) extends TrigF1 { // ( e:Expr ) extends TrigF1 with Expr {
	override def toString() = "sin(" + e.toString() + ")"
	override def eval() = e.eval() match{
		case Bool( false ) => Number( 0 )
		case Bool( true ) => Number( math.sin( 1 ) )
		case Number( n ) => Number( math.sin( n ) )
		case _:ConstantPi => Number( 0 ) 
		// expression with all variables resolved, but some constants may remain!
	}
	//val inverse = AsinF1

	override def visit(env:Option[Environment]) = e.visit( env ) match {
		case Number( 0 ) => Number( 0 )
		case Product( Number( v ), ConstantPi() ) if( v % 1 == 0 ) => Number( 0 )
		//case ( Product( Number( v ), ConstantPi() ) ) if( v % 1 == 0 ) => Number( -1 )
		case ConstantPi() => Number( 0 )
		case Fraction( ConstantPi(), Number( 2 ) ) => Number( 1 )
		//case ( Product( Number( a ), Fraction( ConstantPi(), Number( b ) ) ) ) if ( b / a ).asInstanceOf[Int] ==  
		case a => SinF1( a )
	}

	def info( env:Option[Environment]=None) = "SinF1(" + e.info() + ")"
}

case class CosF1( e:Expr ) extends TrigF1 {
	override def toString() = "cos(" + e.toString() + ")"
	override def eval() = e.eval() match{
		case Bool( false ) => Number( 1 )
		case Bool( true ) => Number( math.cos( 1 ) )
		case Number( n ) => Number( math.cos( n ) )
		case _:ConstantPi => Number( -1 ) 
		// expression with all variables resolved, but some constants may remain!
	}
	//val inverse:InvTrigF1 = AcosF1

	override def visit(env:Option[Environment]) = e.visit( env ) match {
		case Number( 0 ) => Number( 1 )
		case Product( Number( v ), ConstantPi() ) if( v % 2 == 0 ) => Number( 1 )
		case Product( Number( v ), ConstantPi() ) if( v % 1 == 0 ) => Number( -1 )
		case ConstantPi() => Number( -1 )
		case Fraction( ConstantPi(), Number( 2 ) ) => Number( 1 )
		//case ( Product( Number( a ), Fraction( ConstantPi(), Number( b ) ) ) ) if ( b / a ).asInstanceOf[Int] ==  
		case a => CosF1( a )
	}

	def info( env:Option[Environment]=None) = "CosF1(" + e.info() + ")"
}

case class TanF1( e:Expr ) extends TrigF1{
	override def toString() = "tan(" + e.toString() + ")"
	override def eval() = e.eval() match{
		case Bool( false ) => Number( 0 )
		case Bool( true ) => Number( math.tan( 1 ) )
		case Number( n ) => Number( math.tan( n ) )
		//case _:ConstantPi => Number( 0 ) 
		// expression with all variables resolved, but some constants may remain!
	}
	//val inverse:InvTrigF1 = AtanF1

	override def visit(env:Option[Environment]) = ( e.visit( env ) ) match {
		case ( Number( 0 ) ) => Number( 0 )
		case ( Product( Number( v ), ConstantPi() ) ) if( v % 1 == 0 ) => Number( 0 )
		//case ( Product( Number( v ), ConstantPi() ) ) if( v % 1 == 0 ) => Number( -1 )
		case ( ConstantPi() ) => Number( 0 )
		case ( Fraction( ConstantPi(), Number( 2 ) ) ) => Number( 1 )
		//case ( Product( Number( a ), Fraction( ConstantPi(), Number( b ) ) ) ) if ( b / a ).asInstanceOf[Int] ==  
		case ( a ) => TanF1( a )
	}

	def info( env:Option[Environment]=None) = "TanF1(" + e.info() + ")"
}

case class AcosF1( e:Expr ) extends AtrigF1 {
	override def toString() = "acos(" + e.toString() + ")"
	def info(env:Option[Environment]=None) = "AcosF1(" + e.info() + ")"
}

case class AsinF1( e:Expr ) extends AtrigF1 {
	override def toString() = "asin(" + e.toString() + ")"
	def info(env:Option[Environment]=None) = "AsinF1(" + e.info() + ")"
}

case class AtanF1( e:Expr ) extends AtrigF1 {
	override def toString() = "atan(" + e.toString() + ")"
	def info(env:Option[Environment]=None) = "AtanF1(" + e.info() + ")"
}
