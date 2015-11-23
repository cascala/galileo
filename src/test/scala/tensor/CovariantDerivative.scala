import org.scalatest._

import util.Random //. Random

import galileo.expr.{Number,Power,Product,Square,Variable}
//import galileo.linalg.EyeMatrix
import galileo.tensor._
import galileo.trigonometry.{CosF1,SinF1}

class CovariantDerivativeTest extends FunSuite {
	import TensorIndexKind._

	val r = Variable( "r" )
	val theta  = Variable( "theta" ) 
	val Va = Tensor( 
		List( TensorIndex( Upper, 2) ), 
		List( 
			Product( Power( r, Number( 2 ) ), CosF1( theta ) ),
			Product( Number( -1 ), SinF1( theta ) )
		)
	)

	test( "CommaDerivative" ){
		assert( CommaDerivative( Va, r ).valueAt( 0 ).toString == "2.0*r*cos(theta)" )
		assert( CommaDerivative( Va, r ).valueAt( 1 ).toString == "0.0" )
		assert( CommaDerivative( Va, theta ).valueAt( 1 ).toString == "(-1.0)*cos(theta)" )
	}

	// metric for polar coordinates 
	// ds^2=d(r)^2+r^2 *d(theta)^2
	val metric = Metric(
		List( "r", "theta" ).map( x => Variable( x ) ),
		List( 
			Number( 1 ), Number( 0 ),
			Number( 0 ), Square( Variable( "r" ) )
		),
		Lower
	)

	// Semi-colon derivative
	test( "CovariantDerivative") {
		val cs = ChristoffelSecond( metric )
		assert( cs.valueAt(0,1,1).toString == "(-1.0)*r" )
		assert( cs.valueAt(1,1,0).simplify.toString == "1.0/r" )
		assert( cs.valueAt(1,1,0) == cs.valueAt( 1,0,1 ) )
		val pd = CommaDerivative(metric, Va)
		//info( "m*v:" + (cs*Va).contract(3,2))
		val cd = CovariantDerivative( metric, Va )
		assert( cd.rankInt == Va.rankInt+1 )
		assert( cd.valueAt(0,0).toString == "2.0*r*cos(theta)" ) // Vr,r
		assert( cd.valueAt(0,1).toString == "(-1.0)*r*sin(theta)/r^2.0") // Vr,theta
	}

}
