import org.scalatest._

import util.Random //. Random

import galileo.environment.Environment
import galileo.expr.{Number,Sum, Variable}
import galileo.tensor._

class WeylTest extends FunSuite {
	import TensorIndexKind._

	test( "three-sphere" ) {
		val m = Metric.threeSphere( Number( 1 ) )
		val C = WeylTensor( m )
		assert( 1 == 1 )
		// Bianchi identity
		// 4-tuple with all valid indices (i,j,k,l) all from 0 until dimension
		val env = new Environment(None)
		val rng = new Random()
		env.set( "psi",   Number( rng.nextDouble() ) )
		env.set( "theta", Number( rng.nextDouble() ) )
		//info( "env:" + env)
		val indices = 0 until m.dimension

        // Testing the Bianchi identity
		for( i <- indices; j <- indices; k <- indices; l <- indices ) {
			assert( Sum( C.valueAt( i, j, k, l ), C.valueAt( k, i, j, l ), C.valueAt( j, k, i, l ) ).visit().simplify == Number( 0 ) )
			
			// Numeric check, before simplification
			val left = C.valueAt( i, j, k, l ).visit(Some(env)).eval
			val right = C.valueAt( k, l, i, j ).visit(Some(env)).eval
			// Numeric check, after simplification
			val le = C.valueAt( i, j, k, l ).simplify.visit(Some(env)).eval
			val ri = C.valueAt( k, l, i, j ).simplify.visit(Some(env)).eval
			(left, right) match {
				case (Number(x),Number(y)) => assert( math.abs(x-y)< 1E-8)
				case _ => assert( C.valueAt( i, j, k, l ).simplify.visit(Some(env)).eval == C.valueAt( k, l, i, j ).simplify.visit(Some(env)).eval, "i,j,k,l:" + i + "," + j + "," + k + "," + l )
			}
			(le, ri) match {
				case (Number(x),Number(y)) => assert( math.abs(x-y)< 1E-8)
				case _ => assert( C.valueAt( i, j, k, l ).simplify.visit(Some(env)).eval == C.valueAt( k, l, i, j ).simplify.visit(Some(env)).eval, "i,j,k,l:" + i + "," + j + "," + k + "," + l )
			}
			(left, le) match {
				case (Number(x),Number(y)) => assert( math.abs(x-y)< 1E-8, C.valueAt(i,j,k,l))
				case _ => assert( C.valueAt( i, j, k, l ).simplify.visit(Some(env)).eval == C.valueAt( k, l, i, j ).simplify.visit(Some(env)).eval, "i,j,k,l:" + i + "," + j + "," + k + "," + l )
			}
			(right, ri) match {
				case (Number(x),Number(y)) => assert( math.abs(x-y)< 1E-8)
				case _ => assert( C.valueAt( i, j, k, l ).simplify.visit(Some(env)).eval == C.valueAt( k, l, i, j ).simplify.visit(Some(env)).eval, "i,j,k,l:" + i + "," + j + "," + k + "," + l )
			}
			
			assert( Sum( C.valueAt( i, j, k, l ), C.valueAt( j, i, k, l ) ).visit().simplify == Number( 0 ) )
		}
	}
}