package galileo.trigonometry

import galileo.constants._
import galileo.environment.Environment
import galileo.expr._
import galileo.linalg.Matrix
import galileo.logic.Bool

trait TrigF1 extends FunF1 {
	val operation:Double=>Double
	def evalBase() = e.eval() match {
		case Number( n ) => Number( operation( n ) )
		case _:Expr => this
	}
}

trait AtrigF1 extends TrigF1 {
	override def evalBase() = e.eval() match {
		case Number( n ) if n > 1.0 => ErrorExpr( "Can't take "  + this.toString() + " with operand >1 (" + n + ")" )
		case Number( n ) if n < -1.0 => ErrorExpr( "Can't take " + this.toString() + " with operand <-1 (" + n + ")" )
		case Number( n ) => Number( operation( n ) )
		case _:Expr => this
	}
}

case class SinF1( e:Expr ) extends TrigF1 { // ( e:Expr ) extends TrigF1 with Expr {
	override def toString() = "sin(" + e.toString() + ")"
	val operation = { d => math.sin( d ) }
	override def eval() = e.eval() match {
		case Bool( false ) => Number( 0 )
		case Bool( true ) => Number( math.sin( 1 ) )
		case _:ConstantPi => Number( 0 ) 
		case _ => evalBase()
		// expression with all variables resolved, but some constants may remain!
	}

	override def visit(env:Option[Environment]) = e.visit( env ) match {
		case Number( 0 ) => Number( 0 )
		case Product( Number( v ), ConstantPi() ) if( v % 1 == 0 ) => Number( 0 )
		//case ( Product( Number( v ), ConstantPi() ) ) if( v % 1 == 0 ) => Number( -1 )
		case ConstantPi() => Number( 0 )
		case Fraction( ConstantPi(), Number( 2 ) ) => Number( 1 )
		case m:Matrix => m.operate(env,e=>SinF1(e)) // elementwise operation
		//case ( Product( Number( a ), Fraction( ConstantPi(), Number( b ) ) ) ) if ( b / a ).asInstanceOf[Int] ==  
		case a => SinF1( a )
	}

	def info( env:Option[Environment]=None) = "SinF1(" + e.info() + ")"
}

case class CosF1( e:Expr ) extends TrigF1 {
	override def toString() = "cos(" + e.toString() + ")"
	val operation = { d => math.cos( d ) }
	override def eval() = e.eval() match{
		case Bool( false ) => Number( 1 )
		case Bool( true ) => Number( math.cos( 1 ) )
		case _:ConstantPi => Number( -1 ) 
		case _ => evalBase()
	}

	override def visit(env:Option[Environment]) = e.visit( env ) match {
		case Number( 0 ) => Number( 1 )
		case Product( Number( v ), ConstantPi() ) if( v % 2 == 0 ) => Number( 1 )
		case Product( Number( v ), ConstantPi() ) if( v % 1 == 0 ) => Number( -1 )
		case ConstantPi() => Number( -1 )
		case Fraction( ConstantPi(), Number( 2 ) ) => Number( 1 )
		case m:Matrix => m.operate(env,e=>CosF1(e)) // elementwise operation
		//case ( Product( Number( a ), Fraction( ConstantPi(), Number( b ) ) ) ) if ( b / a ).asInstanceOf[Int] ==  
		case a => CosF1( a )
	}

	def info( env:Option[Environment]=None) = "CosF1(" + e.info() + ")"
}

case class TanF1( e:Expr ) extends TrigF1{
	override def toString() = "tan(" + e.toString() + ")"
	val operation = { d => math.tan( d ) }
	override def eval() = e.eval() match{
		case Number( 0 ) => Number( 0 )
		case Bool( false ) => Number( 0 )
		case Number( 1 ) => Number( math.tan( 1 ) )
		case Bool( true ) => Number( math.tan( 1 ) )
		case _ => evalBase()	
	}

	override def visit(env:Option[Environment]) = ( e.visit( env ) ) match {
		case ( Number( 0 ) ) => Number( 0 )
		case ( Product( Number( v ), ConstantPi() ) ) if( v % 1 == 0 ) => Number( 0 )
		//case ( Product( Number( v ), ConstantPi() ) ) if( v % 1 == 0 ) => Number( -1 )
		case ( ConstantPi() ) => Number( 0 )
		case ( Fraction( ConstantPi(), Number( 2 ) ) ) => Number( 1 )
		case m:Matrix => m.operate(env,e=>TanF1(e)) // elementwise operation
		//case ( Product( Number( a ), Fraction( ConstantPi(), Number( b ) ) ) ) if ( b / a ).asInstanceOf[Int] ==  
		case ( a ) => TanF1( a )
	}

	def info( env:Option[Environment]=None) = "TanF1(" + e.info() + ")"
}

case class AcosF1( e:Expr ) extends AtrigF1 {
	override def toString() = "acos(" + e.toString() + ")"
	def info(env:Option[Environment]=None) = "AcosF1(" + e.info() + ")"
	val operation = { d => math.acos( d ) }
	override def eval() = e.eval() match {
		case Number( 0 ) => Fraction( ConstantPi(), Number( 2 ) ).eval()
		case Bool( false ) => Fraction( ConstantPi(), Number( 2 ) ).eval()
		case Number( 1 ) => Number( 0 )
		case Bool( true ) => Number( 0 )
		case _ => evalBase() 
	}
}

case class AsinF1( e:Expr ) extends AtrigF1 {
	override def toString() = "asin(" + e.toString() + ")"
	def info(env:Option[Environment]=None) = "AsinF1(" + e.info() + ")"
	val operation = { d => math.asin( d ) }
	override def eval() = e.eval() match {
		case Number( 0 ) => Number( 0 ) 
		case Bool( false ) => Number( 0 )
		case Number( 1 ) => Fraction( ConstantPi(), Number( 2 ) ).eval()
		case Bool( true ) => Fraction( ConstantPi(), Number( 2 ) ).eval()
		case _ => evalBase() 
	}
}

case class AtanF1( e:Expr ) extends AtrigF1 {
	override def toString() = "atan(" + e.toString() + ")"
	def info(env:Option[Environment]=None) = "AtanF1(" + e.info() + ")"
	val operation = { d => math.atan( d ) }
	override def eval() = e.eval() match {
		case Number( 0 ) => Number( 0 ) 
		case Bool( false ) => Number( 0 )
		case Number( 1 ) => Fraction( ConstantPi(), Number( 4 ) ).eval()
		case Bool( true ) => Fraction( ConstantPi(), Number( 4 ) ).eval()
		case _ => evalBase() 
	}
}
