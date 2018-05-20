import org.scalatest._

import util.Random //. Random

import galileo.expr.{Number,Variable}
import galileo.tensor._

class RiemannTest extends FunSuite {
	import TensorIndexKind._

	test( "twoSphere" ) {
		val m2 = Metric.twoSphere( Variable( "r" ) )

		// Christoffel symbol of first kind
		val cf = ChristoffelFirst( m2 )
		assert( cf.valueAt(0,1,1).simplify.toString == "r^2.0*cos(theta)*sin(theta)" )
		assert( cf.valueAt(1,1,0).simplify.toString == "(-1.0)*r^2.0*cos(theta)*sin(theta)" )
		// Christoffel symbol of second kind
		val cs = ChristoffelSecond( m2 )
		assert( cs.valueAt(0,1,1).simplify.toString == "(-1.0)*cos(theta)*sin(theta)" )
		assert( cs.valueAt(1,1,0).simplify.toString == "cos(theta)/sin(theta)" )
		assert( cs.valueAt(1,0,1).simplify.toString == "cos(theta)/sin(theta)" )

		// covariant Riemann
		val rf = RiemannFirst( m2 ) // R_abcd
		assert(rf.valueAt( 1,0,1,0).simplify.toString == "r^2.0*sin(theta)^2.0" )
		assert(rf.valueAt( 1,0,0,1).simplify.toString == "(-1.0)*r^2.0*sin(theta)^2.0" )

		// the simplification is a bit messy...
		val rs = RiemannSecond( m2 ) // R^a_bcd
		assert(rs.valueAt( 0,1,0,1).expand.simplify.toString == "sin(theta)^2.0" )

		// covariant Ricci R_ab
		// the values do not tie with the CRC book
		val ricci = RicciTensor(m2)
		assert( ricci.valueAt(0,0).simplify.toString == "1.0" )
		assert( ricci.valueAt(1,1).simplify.toString == "sin(theta)^2.0" ) // 
		assert( ricci.valueAt(0,1) == Number( 0 ))

		// this does not tie with the CRC book
		val ricciScalar = RicciScalar( m2 )
		assert( ricciScalar.valueAt().simplify.toString == "2.0/r^2.0" )

		// Einstein tensor and 'trace' (i.e. scalar)
		val einsteinTensor = EinsteinTensor( m2 )
		// always zero in two dimensions
		assert( einsteinTensor.valueAt(0,0).simplify.toString == "0.0" )
		assert( einsteinTensor.valueAt(0,1).simplify.toString == "0.0" )
		assert( einsteinTensor.valueAt(1,0).simplify.toString == "0.0" )
		assert( einsteinTensor.valueAt(1,1).simplify.visit().toString == "0.0" )	
		val einsteinScalar = EinsteinScalar( m2 )
		assert( einsteinScalar.valueAt().simplify.toString == "0.0" )

	}

	// 3-sphere metric
	// https://books.google.com/books?id=ZIJhsqo9O7MC&lpg=PA369&ots=8T5tpmNx8o&dq=Christoffel%20symbols%202-sphere&pg=PA369#v=onepage&q=Christoffel%20symbols%202-sphere&f=false
	val m3 = Metric.threeSphere( Variable( "r" ) ) // psi, theta, phi
	test( "threeSphere") {
		assert( m3.toLower.valueAt(0,0).toString == "r^2.0" )
		assert( m3.toLower.valueAt(1,1).toString == "r^2.0*sin(psi)^2.0" )
		assert( m3.toLower.valueAt(2,2).toString == "r^2.0*sin(psi)^2.0*sin(theta)^2.0" )
		assert( m3.toUpper.valueAt(0,0).toString == "1.0/r^2.0" )
		assert( m3.toUpper.valueAt(1,1).toString == "1.0/(r^2.0*sin(psi)^2.0)" )
		assert( m3.toUpper.valueAt(2,2).toString == "1.0/(r^2.0*sin(psi)^2.0*sin(theta)^2.0)" )
		val cf = ChristoffelFirst( m3 ) // G_abc
		assert( cf.valueAt(1,1,0).simplify.toString == "(-1.0)*r^2.0*cos(psi)*sin(psi)" )	
		assert( cf.valueAt(0,1,1).simplify.toString == "r^2.0*cos(psi)*sin(psi)" )
		assert( cf.valueAt(0,2,2).simplify.toString == "r^2.0*sin(theta)^2.0*cos(psi)*sin(psi)" )
		assert( cf.valueAt(2,2,0).simplify.toString == "(-1.0)*r^2.0*sin(theta)^2.0*cos(psi)*sin(psi)" )
		assert( cf.valueAt(2,2,1).simplify.toString == "(-1.0)*r^2.0*sin(psi)^2.0*cos(theta)*sin(theta)" )
		assert( cf.valueAt(1,2,2).simplify.toString == "r^2.0*sin(psi)^2.0*cos(theta)*sin(theta)" )
		val cs = ChristoffelSecond( m3 ) // G^a_bc
		assert( cs.valueAt(0,1,1).simplify.toString == "(-1.0)*cos(psi)*sin(psi)" )
		assert( cs.valueAt(1,0,1).simplify.toString == "cos(psi)/sin(psi)" ) // cot(psi)
		assert( cs.valueAt(2,0,2).simplify.toString == "cos(psi)/sin(psi)" ) // cot(psi)
		
		assert( cs.valueAt(0,2,2).simplify.toString == "(-1.0)*sin(theta)^2.0*cos(psi)*sin(psi)" ) 
		assert( cs.valueAt(1,2,2).simplify.toString == "(-1.0)*cos(theta)*sin(theta)" ) 
		assert( cs.valueAt(2,1,2).simplify.toString == "cos(theta)/sin(theta)" ) // cot(theta)

		val rf = RiemannFirst( m3 )
		assert( rf.valueAt(0,1,0,1).simplify.toString == "r^2.0*sin(psi)^2.0" )
		assert( rf.valueAt(0,2,0,2).simplify.toString == "r^2.0*sin(psi)^2.0*sin(theta)^2.0" )
		/*
		info( "Without anyting:" + rf.valueAt(1,2,1,2).toString )
		info( "with simplify.factor:" + rf.valueAt(1,2,1,2).simplify.factor.toString )
		info( "with simplify.factor.factor:" + rf.valueAt(1,2,1,2).simplify.factor.factor )
		info( "with simplify:" + rf.valueAt(1,2,1,2).simplify.toString )
		info( "Without visit:" + rf.valueAt(1,2,1,2).simplify.factor.toString )
		info( "With visit:" + rf.valueAt(1,2,1,2).simplify.factor.visit().toString )
		*/
		//println( "to be simplified: " + rf.valueAt(1,2,1,2) )
		assert( rf.valueAt(1,2,1,2).simplify.factor.visit().toString == "sin(psi)^4.0*r^2.0*sin(theta)^2.0" )	

		// this does NOT tie with the CRC book
		val ricci = RicciTensor( m3 )	 // covariant Ricci R_ab
		assert( ricci.valueAt(0,0).simplify.toString == "2.0" )
		//println( "info: " + ricci.valueAt(1,1).simplify.info() )
		assert( ricci.valueAt(1,1).simplify.visit().visit().toString == "2.0*sin(psi)^2.0" )
		assert( ricci.valueAt(2,2).simplify.factor.visit().toString == "2.0*sin(psi)^2.0*sin(theta)^2.0" )	
		
		val ricciScalar = RicciScalar( m3 )
		//info( "Ricci Scalar:\n" + ricciScalar.valueAt().simplify.toString )
		assert( ricciScalar.valueAt().simplify.toString == "6.0/r^2.0" )

		val einsteinTensor = EinsteinTensor( m3 )
		assert(einsteinTensor.valueAt(0,0).simplify.visit().toString == "-1.0" )
		assert(einsteinTensor.valueAt(0,1).simplify.visit().toString == "0.0" )
		assert(einsteinTensor.valueAt(1,0).simplify.visit().toString == "0.0" )
		assert(einsteinTensor.valueAt(1,1).simplify.visit().toString == "(-1.0)*sin(psi)^2.0" )						

		/* Something wrong... or simplify needs more work :(
		// related to Ricci tensor by G = (2-n)/2*R, with n the number of dimensions
		// For n = 3, G = (2-3)/2*R = -R/2 = -6/r^2/2 = -3/r^2
		val einsteinScalar = EinsteinScalar( m3 )
		assert( einsteinScalar.valueAt().simplify.expand.factor.simplify.visit().visit().visit().toString == "(-3.0)/r^2.0")
		*/
	}
}
