package galileo.complex

import galileo.constants.ConstantPi
import galileo.environment.Environment
import galileo.expr._
import galileo.trigonometry.{SinF1,CosF1,AtanF1}

case class Complex(real:Expr, imag:Expr) extends Expr {
	def +(that:Number) = Complex( Sum( this.real, that ), this.imag )
	def +(that:Complex) = Complex( Sum( this.real, that.real ), Sum( this.imag, that.imag ) )
	//override def +(that:Expr) = Complex( Sum(this.real, that), this.imag )
	def *(that:Number) = Complex( Product( this.real, that ), Product( this.imag, that ) )
	def *(that:Complex) = Complex( 
		Sum( Sum( this.real, that.real ), Product( Number(-1), Product( this.imag, that.imag ) ) ),
		Sum( Product( this.real, that.imag ), Product( this.imag, that.real ) )
	)

	// don't call visit here... Visit should only be called in visit functions
	def ^(that:Number) = (real, imag ) match {
		case ( Number( 0 ), i:Expr ) if (that.value%4 == 0 ) => Power( i, that ) // of for pos and neg multiples of 4
		case ( Number( 0 ), i:Expr ) if (that.value%2 == 0 ) => Product( Number( -1 ), Power( i, that ) ) // of for pos and neg multples of 2
		case ( Number( 0 ), i:Expr ) if ((that.value+1)%4==0) => Complex( Number( 0 ), Product( Number( -1 ), Power( i, that ) ) )
		case ( Number( 0 ), i:Expr ) if ((that.value-1)%4==0) => Complex( Number( 0 ), Power( i, that ) )
		case ( r:Number, i:Number ) => Complex( 
			Product( modulus, CosF1( Product( that, this.phase ) ) ),
			Product( modulus, SinF1( Product( that, this.phase ) ) )
		)
		case _ => Power( this, that ) // this will cause a cycle if called 
	}
	
	val modulus = RootN( Sum( 
		Power( real, Number( 2 ) ),
		Power( imag, Number( 2 ) )
	), Number( 2 ) )

	val phase = (real, imag) match {
		case ( Number( 0 ), Number( 0 ) ) => Number( 0 )
		case ( Number( 0 ), im ) if ( im > 0 )  => Fraction( ConstantPi(), Number( 2 ) )
		case ( Number( 0 ), im ) if ( im < 0  ) => Product( Number( -1 ), Fraction( ConstantPi(), Number( 2 ) ) )
		case (_,_ ) => AtanF1( Fraction( imag, real ) )
	}

	// a * e^(i*theta) = a * cos( theta ) + a * i * sin( theta )
	// a * e^(n*i*theta ) = a * cos( n * theta ) + a * i * sin ( n * theta )
	// if that decomp does not exist...
	// expr + i * expr..., then what?
	/*def ^(that:Number) = {
		Complex( )
	}
	*/

	override def toString() = (real, imag ) match {
		case ( a, Number( 0 ) ) => a.toString()
		case ( Number( 0 ), Number( 1 ) ) => "j"
		case ( Number( 0 ), Number( -1 ) ) => "-j"
		case ( Number( 0 ), a ) => "j * " + a.factorToString()
		case ( a, Number(  1 ) ) => a.toString() + "+j"
		case ( a, Number( -1 ) ) => a.toString() + "-j"
		case ( _,_ ) => real.toString() + "+j*" + imag.factorToString() 
	}
	def info(env:Option[Environment]=None) = "Complex(" + real.info(env) + "," + imag.info( env ) + ")"
		// the way to print a complex expression if it is used as a factor
		// e.g. ( 6 + j ) in ( 6 + j ) * a
	override def factorToString():String = (real,imag) match {
			case ( Number( 0 ), a ) => this.toString()
			case ( a, Number( 0 ) ) => this.toString()
			case (_,_) => "(" + this.toString() + ")" 
	} 

	override def visit( env:Option[Environment]=None):Expr = ( real.visit( env ), imag.visit( env ) ) match {
		case ( r:Expr, Number( 0.0 ) ) => r
		case ( r, i ) => Complex( r, i )
	}
}
