import org.scalatest._

import util.Random
import util.Random.nextDouble
import scala.collection.mutable.ListBuffer
import galileo.expr._
import galileo.linalg._

object Mapper{
	def apply( l:List[List[Double]] ):List[List[Expr]] = l.map( row => row.map( elem => Number( elem ) ) )
}

class LinAlgTest001 extends FunSuite {
	//def mapper( l:List[List[Double]] ):DenseMatrix = DenseMatrix( l.map( row => row.map( elem => Number( elem ) ) ) )
	val a_3 = DenseMatrix( Mapper( List( 
		List( 8, 2, 3 ),
		List( 7, 5, 6 ),
		List( 6, 4, 9 )
	) ) )

	val b_2 = DenseMatrix( Mapper( List( 
		List( 5, 6 ),
		List( 4, 7 )
	) ) )

	val c_2 = DenseMatrix( Mapper( List( 
		List( 5, 6 ),
		List( 8, 7 )
	) ) )

	val d_4 = DenseMatrix( Mapper( List( 
		List( 4, 3, 2, 1 ),
		List( 1, 2, 3, 4 ),
		List( 5, 6, 7, 9 ),
		List( 9, 7, 6, 6 )
	) ) )

	val e_2 = DenseMatrix( Mapper( List( 
		List( -1, 0 ),
		List(  0, 1 )
	) ) )

	test( "inverse") {
		matChecker( e_2.inverse * e_2, EyeMatrix( 2 ).toDenseMatrix, 2 )
	}

	test( "MatSolve")
	{
		checker( a_3, 3 )
		checker( b_2, 2 )
		checker( c_2, 2 )
		checker( d_4, 4 )
	}

	def matChecker(a:DenseMatrix,b:DenseMatrix, numCols:Int, tol:Double=1e-12):Unit = {
		assert( a.numRows == b.numRows )
		assert( a.numCols == b.numCols )
		assert( a.numCols == numCols )
		val ar = a.rows;
		val br = b.rows;
		for( i <- 0 until a.numRows )
		{
			for( j <- 0 until a.numCols )
			{
				val ae = ar(i)(j)
				var be = br(i)(j)
				(ae.eval(), be.eval()) match {
					case ( Number( an ), Number( bn ) ) => assert( math.abs( an - bn ) < tol )
					case (_,_) => assert( ae == be )
				}
			}
		}
	}

	val rng = new Random()
	def checker( mat:DenseMatrix, sz:Int ): Unit = {
		assert( mat.numRows == sz )
		assert( mat.numCols == sz )

		def colChecker(nc:Int=1): Unit = {
			var rhs = ( 0 until sz ).map( elem => List.fill(nc){elem} ) //.to[ListBuffer]
			val b1 = DenseMatrix( Mapper( rhs.map( row => row.map( elem => ( 5 + elem ) * rng.nextDouble() ) ).to(List) ) )

			val x = mat.solve( b1 )
			val b2 = ( mat * x ).visit()
			matChecker( b1, b2, nc )

			val (l,u,p) = mat.lup_lup		
			
			// recreate solution
			val pa1 = p * mat.visit()
			val pa2 = l.toDenseMatrix * u.toDenseMatrix
			matChecker( pa1, pa2, mat.numCols )

			val (l1,u1) = mat.lu_lup
			val mat2 = l1.toDenseMatrix * u1.toDenseMatrix
			matChecker( mat, mat2, mat.numCols )

			// apply P, then L solve then U solve
			val x2 = u.solve( l.solve( p * b1 ) )
			matChecker( x, x2, nc )

			// check features of L, U, and P
			val identity = EyeMatrix( sz ).toDenseMatrix
			matChecker( identity, p.inverse * p.toDenseMatrix, sz )
			matChecker( identity, p * p.inverse.toDenseMatrix, sz )
			val (ll, lu, lp) = l.lup_lup
			val (ul, uu, up) = u.lup_lup
			val (pl, pu, pp) = p.lup_lup
			matChecker( lp * l.toDenseMatrix, ll * lu.toDenseMatrix, sz )
			matChecker( up * u.toDenseMatrix, ul * uu.toDenseMatrix, sz )
			matChecker( pp * p.toDenseMatrix, pl * pu.toDenseMatrix, sz )
		}

		colChecker( 1 )
		colChecker( 2 )
		colChecker( 3 )
		colChecker( 4 )
		colChecker( 5 )
	}
	
	//(L,U,P) = 
	//assert( Number( 0 ) == Complex( Number( 10 ), Number( 5 ) ) * Number( 0 ) )
} // class ExprTest001

class LinAlgTest002 extends FunSuite {
	// l = [
	//   1, 0
	//   2, 3
	// ]
	val l1 = LowerTriangularMatrix( Mapper( List(
		List( 1, 2), 
		List( 3 )
	) ) )
	val l2 = DenseMatrix( Mapper( List(
		List( 1, 0 ), 
		List( 2, 3 )
	) ) )
	test( "LowerTriangular denseMatrix"){
		assert( l1.toDenseMatrix == l2 )
		assert( l2.toDenseMatrix == l2 )
	}

	val u1 = UpperTriangularMatrix( Mapper( List(
		List( 1, 2), 
		List( 3 )
	) ) )
	val u2 = DenseMatrix( Mapper( List(
		List( 1, 2 ), 
		List( 0, 3 )
	) ) )
	test( "UpperTriangular denseMatrix"){
		assert( u1.toDenseMatrix == u2 )
		assert( u2.toDenseMatrix == u2 )
	}

}	
