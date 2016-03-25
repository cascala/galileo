package galileo.tensor

import galileo.environment.Environment
import galileo.expr._
import galileo.trigonometry.SinF1

import TensorIndexKind._

object Metric {
	def apply( variables:List[Variable], components:List[Expr], kind:TensorIndexKind=Upper ):Metric = {
		require( variables.size * variables.size == components.size )
		Metric( 
			Tensor( 
				List( TensorIndex( kind, variables.size ), TensorIndex( kind, variables.size ) ),
				components
			),
			variables
		)
	}

	// x^2+y^2+z^2=r^2
	// x = r*sin(theta)*cos(phi)
	// y = r*sin(theta)*sin(phi)
	// z = r*cos(theta)
	// ds^2=r^2*dtheta^2+r^2*sin(theta)^2*dphi^2
	def twoSphere(radius:Expr):Metric = Metric(
		Tensor( List( Lower, Lower ), 2, 
			List( Square( radius ), Number( 0 ), Number( 0 ), Product( Square( radius ), Square( SinF1( Variable ( "theta" ) ) ) ) )
		),
		List( "theta", "phi " ).map( x => Variable( x ) )
	)

	// x^2+y^2+z^2+w^2=r^2
	// x = r*sin(psi))*sin(theta)*cos(phi)
	// y = r*sin(psi)*sin(theta)*sin(phi)
	// z = r*ssn(psi)*cos(theta)
	// z = r*cos(psi)
	def threeSphere(radius:Expr):Metric = Metric(
		Tensor( List( Lower, Lower ), 3, 
			List( 
				Square( radius ), Number( 0 ), Number( 0 ), 
				Number( 0 ), Product( Square( radius ), Square( SinF1( Variable ( "psi" ) ) ) ), Number( 0 ),
				Number( 0 ), Number( 0 ), Product( Square( radius ), Square( SinF1( Variable ( "psi" ) ) ), Square( SinF1( Variable ( "theta" ) ) ) )
			)
		),
		List( "psi", "theta", "phi " ).map( x => Variable( x ) )
	)

	// Schwarzschild metric
	def schwarzschild(radius:Expr):Metric = {
		val r_s:Expr= Variable("r_s") // schwarzschild radius,  2*G*M/c^2
		Metric(
			Tensor( 
				List( Lower, Lower ), 4, // 4 dimensional
				List( 
					Product( Number( -1 ), Fraction( radius - r_s, radius ) ), Number( 0 ), Number( 0 ), Number( 0 ),
					Number( 0 ), Fraction( radius, radius - r_s ), Number( 0 ), Number( 0 ),
					Number( 0 ), Number( 0 ), Square( radius ), Number( 0 ),
					Number( 0 ), Number( 0 ), Number( 0 ), Product( Square( radius ), Square( SinF1( Variable( "phi" ) ) ) )
				)
			),
			List( "t", "r", "theta", "phi" ).map( x => Variable( x ) )
		)
	}
}

// g^ab
case class Metric private(tensor:Tensor,variables:List[Variable]) extends Expr {
	def info(env:Option[Environment]=None) = "Metric(" + tensor + ",[" + variables.mkString(",") + "])"
	def inverse:Metric = {
		val indices = tensor.indices.map( index => index.inverse )
		val m = tensor.toMatrix.inverse.toDenseMatrix.rows.flatten
		Metric( Tensor( indices, m ), variables )
	}
	val dimension = variables.size
	val kind = tensor.indices(0).kind

	// g_ab
	def toLower:Metric = kind match {
		case Lower => this
		case Upper => this.inverse
	}

	// g^ab
	def toUpper:Metric = kind match {
		case Lower => this.inverse
		case Upper => this
	}

	def *(that:Christoffel) = this.tensor * that.tensor
	def *(that:Tensor) = this.tensor * that
	
	def valueAt( location:Int* ):Expr = this.tensor.valueAt( location:_*)
}
