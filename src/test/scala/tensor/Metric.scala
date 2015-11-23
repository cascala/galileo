import org.scalatest._

import util.Random //. Random

import galileo.expr.{Number,Power,Product,Variable}
import galileo.linalg.EyeMatrix
import galileo.tensor._
import galileo.trigonometry.SinF1

class MetricTest extends FunSuite {
	import TensorIndexKind._

	val m = Metric(
		List( Variable( "t" ), Variable( "x" ), Variable( "y" ), Variable( "z" ) ),  
		List( // metric
			Number( 1 ), Number( 0 ),  Number( 0 ),  Number( 0 ),
			Number( 0 ), Number( -1 ), Number( 0 ),  Number( 0 ),
			Number( 0 ), Number( 0 ),  Number( -1 ), Number( 0 ),
			Number( 0 ), Number( 0 ),  Number( 0 ),  Number( -1 )
		)
	)

	// 2-sphere of radius a
	// lower
	val gl = Metric( 
		List( Variable( "theta" ), Variable( "phi") ), 
		List( 
			Power( Variable( "a" ), Number( 2 ) ), Number( 0 ),
			Number( 0 ), Product( Power( Variable( "a" ), Number( 2 ) ), Power( SinF1( Variable( "theta" ) ), Number( 2 ) ) )
		),
		Lower
	)

	test( "inverse") {
		assert( m.tensor.components == m.inverse.tensor.components ) // it's
		for( (a,b) <- m.tensor.indices.zip( m.inverse.tensor.indices ) )
			assert( a == b.inverse )

		val gu = gl.inverse
		assert( gu.kind == Upper )
		assert( gu.valueAt( 0,0 ).toString == "1.0/a^2.0" )	
		assert( gu.valueAt( 0,1 ).toString == "0.0" )	
		assert( gu.valueAt( 1,0 ).toString == "0.0" )	
		assert( gu.valueAt( 1,1 ).toString == "1.0/(a^2.0*sin(theta)^2.0)" )	
	}
}
