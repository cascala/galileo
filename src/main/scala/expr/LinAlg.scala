package galileo.linalg

// Linear algebra stuff, matrices and vectors
import galileo.expr._
import galileo.environment._

import scala.collection.mutable.{ListBuffer}

trait Matrix extends Expr{
	val numRows:Int
	val numCols:Int
	def *(that:Expr):Expr // dotProduct
	def *(that:DenseMatrix):DenseMatrix
	
	val lu_lup:(Matrix,Matrix) // = this.toDenseMatrix.lu_lup
	val lup_lup:(Matrix,Matrix,Matrix) //= this.toDenseMatrix.lup_lup
	def solve( rhs:DenseMatrix ):DenseMatrix // = DenseMatrix( List( List( Variable( "todo solve") ) ) )
	val toDenseMatrix:DenseMatrix
	val inverse:Matrix
	def visit(env:Option[Environment]):Expr
	def norm(p:Int):Expr = this.toDenseMatrix.norm( p )
	//def tensorProduct(that:Expr):Matrix
	def transpose:Matrix //= this.toDenseMatrix.transpose
	def apply(i:Int,j:Int):Expr = this.toDenseMatrix.rows(i)(j)
}

// for statements like 1:2:11
object LinSpace { 
	def apply( start:Double, end:Double, n:Int=100) =  {
		val incr = ( end - start ) / ( n - 1 ) // todo, use Fraction here
		var i = 0;
		var l:List[Expr] = List()
		while( i < n ) {
			l = l :+ Number( start + incr * i )
			i = i + 1
		}
		DenseMatrix( List( l ) ) //l.map( elem => List( elem ) ) )
	}
}

// to handle b:e and b:i:end
case class RowVector( begin:Expr, end:Expr, incr:Expr ) extends Expr {
	override def visit(env:Option[Environment]=None) = (begin.visit(env), end.visit(env), incr.visit(env)) match {
		case (Number(b),Number(e),Number(i)) => DenseMatrix( List( ( b to e by i ).map( n => Number( n ) ).to[List] ) )
		case (b,e,i) => RowVector(b,e,i)
	}
	def info(env:Option[Environment]=None) = "RowVector(" + begin + "," + end + "," + incr + ")"
}

object LogSpace {
	def apply( start:Double, end:Double, n:Int=50) =  {
		val incr = ( end - start ) / ( n - 1 ) // todo, use fraction here
		var i = 0;
		var l:List[Expr] = List()
		while( i < n ) {
			l = l :+ Power( Number( 10 ), Number( start + incr * i ) ) 
			i = i + 1 
		}
		DenseMatrix( List( l ) ) //l.map( elem => List( elem ) ) )
	}
}

// transpose( expr ) command in parser
case class Transpose(e:Expr) extends Expr {
	def info(env:Option[Environment]=None) = "Transpose(" + e + ")"
	override def visit( env:Option[Environment]=None) = e.visit( env ) match {
		case m:Matrix => m.transpose
		case other => ErrorExpr( "Can not apply transpose to " + e + "(" + other + ") " )
	}
}

// Tensor product
/*
case class MatKron( a:Expr, b:Expr ) extends Expr {
	override def visit(env:Option[Environment]=None) = ( a.visit(env), b.visit(env) ) match {
		case (l:Matrix,r:Matrix) => l.tensorProduct( r )
		case _ => ErrorExpr( "Invalid operands for kron(" + a + "," + b + ")" )
	}
	def info = "MatKron(" + a + "," + b + ")"
}
*/

// U for unhandled - count of rows and cols are expressions, not int
case class OnesMatrixU( nrs:Expr, ncs:Expr,element:Expr=Number(1)) extends Expr {
	override def visit( env:Option[Environment]=None ) = ( nrs.visit(env), ncs.visit(env) ) match {
		case ( Number(nr), Number(nc) ) if (nr%1==0 && nc%1==0) => OnesMatrix( nr.toInt, nc.toInt, element )
		case _ => ErrorExpr( "Incorrect arguments in ones(" + nrs + "," + ncs + ")" ) 
	}
	def info(env:Option[Environment]=None) = "OnesMatrix(" + nrs + "," + ncs + ")"
}

// Matrix with all elements equalt to element
case class OnesMatrix( numRows:Int, numCols:Int, element:Expr=Number( 1 ) ) extends Matrix {
	override def visit( environment:Option[Environment]=None) = 
		OnesMatrix( numRows, numCols, element.visit( environment ) )
	override def toString() = {
		var s = ""
		val elem = element.toString() + "\t"
		for( i <- 0 until numRows ) {
			for( k <- 0 until numCols )
				s = s + elem
			s += "\n"
		}
		s
	}
	def info(env:Option[Environment]=None) = "OnesMatrix todo"

	def *(that:Expr) = OnesMatrix( numRows, numCols, Product( element, that ) )
	//def tensorProduct(that:Expr) = OnesMatrix( numRows, numCols, Product( element, that ) )
	def *(that:DenseMatrix):DenseMatrix = {	//var rows:List[List[Expr]] = Nil
		var sums:List[Expr] = 
			that.rows.transpose.map( column => column.foldLeft( Number( 0 ):Expr ){ case (r:Expr,c:Expr) => Sum( r, c ) } ).map( sum => Product( sum, element ) )
		
		DenseMatrix( List.fill( numRows ){ sums } )
	}
	
	def solve( rhs:DenseMatrix ) = DenseMatrix(List( List( ErrorExpr( "Indefinite matrix can not be solved" ) ) ) )
	
	DenseMatrix( List( List( Variable( "todo solve OnesMatrix") ) ) )
	val toDenseMatrix = DenseMatrix( List.fill(numRows){
		List.fill(numCols){element}
	} )
	lazy val inverse = this.toDenseMatrix.inverse // Is there a more optimal way?
	lazy val lu_lup:(Matrix,Matrix) = this.toDenseMatrix.lu_lup
	lazy val lup_lup:(Matrix,Matrix,Matrix) = this.toDenseMatrix.lup_lup

	def transpose = this
}

case class EyeMatrixU(nrs:Expr,ncs:Expr) extends Expr{
	def info(env:Option[Environment]=None) = "EyeMatrixU(" + nrs + "," + ncs + ")"
	override def visit(env:Option[Environment]=None) = (nrs.visit(env), ncs.visit(env)) match {
		case ( Number( nr ), Number( nc ) ) if ( nr%1==0 && nc%1 == 0 && !(nc < nr )) => EyeMatrix( nr.toInt, nc.toInt )
		case _ => ErrorExpr( "Incorrect arguments in eye(" + nrs + "," + ncs + ")" )
	}
}

object EyeMatrix{
	def apply(n:Integer) = DiagMatrix( n, List.fill(n){ Number( 1 ) } )
	def apply(n:Integer, m:Integer) = { 
		require( m >= n )
		DiagMatrix( m, List.fill(n){ Number( 1 ) } )
	}
}

case class DiagMatrix( numCols:Int, diag:List[Expr] ) extends Matrix {
	override def visit( environment:Option[Environment]=None) = 
		DiagMatrix( numCols, diag.map( element => element.visit( environment ) ) )
	override def toString() = {
		var s = ""
		val zero = Number( 0 ).toString() + "\t"
		for( i <- 0 until diag.size ) {
			var r:List[Expr] = List.fill( numCols ){ Number(0) }
			r = r.updated( i, diag(i) )
			s += r.map( elem => elem.toString() ).mkString( "\t") 
			/*
			for( k <- 0 until numCols ) {
				if( k == i ) s = s + diag( i ).toString() + "\t" else s = s + zero
			}
			*/
			s += "\n"
		}
		s
	}
	def info(env:Option[Environment]=None) = "DiagMatrix todo"
	lazy val numRows = diag.size	
	def *(that:Expr) = DiagMatrix( numCols, diag.map( elem => Product( elem, that ) ) )
	def tensorProduct(that:Expr) = DiagMatrix( numCols, diag.map( elem => Product( elem, that ) ) )

	def *(that:DenseMatrix) = DenseMatrix( 
		that.rows.zip(diag).map( { case ( row, diagelem ) => row.map( elem => Product( elem, diagelem ).visit() ) } )
	)
	//val l_lup = Variable( "todo l_lup")
	override lazy val lu_lup = ( EyeMatrix( this.numRows ), this )
	override lazy val lup_lup = ( EyeMatrix( this.numRows ), this, EyeMatrix( this.numRows ) )

	def solve(rhs:DenseMatrix):DenseMatrix = {
		require( numCols == diag.size )
		var rr = rhs.rows
		for( i <- 0 until numCols )
			rr = rr.updated( i, rr(i).map( elem => Fraction( elem, diag( i ) ).visit() ) )
		DenseMatrix( rr )
	}
	lazy val toDenseMatrix:DenseMatrix = {
		var rows:List[List[Expr]]=Nil
		for( i <- 0 until numRows ) {
			var row:List[Expr] = List.fill(numCols){Number(0)}
			row = row.updated(i,diag(i))
			rows = rows :+ row
		}
		DenseMatrix( rows )
	}

	// todo: How does this work for non square matrices?
	lazy val inverse:DiagMatrix = DiagMatrix( numCols, diag.map( elem => Fraction( Number( 1 ), elem ).visit() ) )
	def transpose = this
}

// cols are only defined starting at the diag element
case class LowerTriangularMatrix( cols:List[List[Expr]] ) extends Expr with Matrix {
	override def toString() = {
		var s:String = ""
		val n = cols(0).size
		for( i <- 0 until n )
		{
			for( j <- 0 to i )
				s = s + cols(j)(i-j) + "\t"
			for( j <- i+1 until n )
				s = s + Number( 0 ) + "\t"
			s = s + "\n"
		}
		s
	}
	def info(env:Option[Environment]=None) = "LowerTriangularMatrix todo"

	override def visit(env:Option[Environment]=None):LowerTriangularMatrix = LowerTriangularMatrix(
		cols.map( col => col.map( elem => elem.visit( env ) ) )
	)

	override def eval() = LowerTriangularMatrix(
		cols.map( col => col.map( elem => elem.eval() ) )
	)
	
	lazy val numCols = cols.size
	lazy val numRows = cols.size

	def *(that:Expr) = {
		LowerTriangularMatrix( cols.map( col => { col.map( elem => Product( elem, that ) ) } ) )
	}

	def tensorProduct(that:Expr) = {
		LowerTriangularMatrix( cols.map( col => { col.map( elem => Product( elem, that ) ) } ) )
	}

	def *(that:DenseMatrix) = this.toDenseMatrix * that.toDenseMatrix

	lazy val toDenseMatrix:DenseMatrix = {
		var rows:List[List[Expr]] = Nil
		for( i <- 0 until numRows )
		{
			var row:List[Expr] = Nil
			for( j <- 0 to i )
				row = row :+ cols(j)(i-j)
			for( j <- i + 1 until numCols )
				row = row :+ Number( 0 )
			rows = rows :+ row //List( row ) :: Nil
		}
		DenseMatrix( rows )
	}

	//val lup = Variable( "todo")
	//val l_lup = Variable( "todo l_lup")
	lazy val lu_lup = ( this, EyeMatrix( this.numRows ) )
	lazy val lup_lup = ( this, EyeMatrix( this.numRows ), EyeMatrix( this.numRows ) )
	
	/*
	* | l11   0 | |x1|   |r1|
	* | l21 l22 | |x2| = |r2|
	*  ...
	*/
	def solve( rhs:DenseMatrix ):DenseMatrix = {
		var x:List[List[Expr]] = Nil//rhs.rows.map( row => row.to[ListBuffer] ).to[ListBuffer]
		// Forward substitution
		for( i <- 0 until this.numRows )
		{
			var xi = rhs.rows( i ).to[ListBuffer]
			for( k <- 0 until xi.size )
			{
				var xik:Expr=xi(k)
				for( j <- 0 until i )
				{
					xik = Sum( xik, Product( Number( -1 ), Product( x(j)(k), cols(j)(i-j) ) ) )
				}
				xi = xi.updated(k, xik)
					//println( "i: " + i + ", j: " + j )
				//x(i) = x(i).map( elem => Sum( elem, Product( Product( Number( -1 ), cols(j)(i-j) ), x(i)(j) ) ) )
			}
			// not strictly needed as diag is always 1...
			x = x :+ xi.map( elem => Fraction( elem, cols(i)(0) ).visit() ).to[List] 
		}
		DenseMatrix( x )
	}
	
	lazy val inverse = this.solve( EyeMatrix( this.numRows ).toDenseMatrix ) //Variable( "Todo")
	def transpose = UpperTriangularMatrix( this.cols )
}

// rows are only defined starting at the diag element
case class UpperTriangularMatrix( rows:List[List[Expr]] ) extends Matrix {
	override def toString() = {
		var s:String = ""
		val n = rows(0).size
		for( i <- 0 until n )
		{
			for( j <- 0 until i )
				s = s + Number( 0 ) + "\t"
			for( j <- i until n )
				s = s + rows(i)(j-i) + "\t"
			
			s = s + "\n"
		}
		s
/*
		def filler(n:Int):List[Expr] = n match{
			case a:Int if ( a <= 0 ) => Nil
			case _ => List.fill(n){ Number(0) }
		}
		var n = rows(0).size - 1
		rows.map( row => { ( filler( n ) :: row ).mkString( "\t" ); n = n - 1 } ).mkString( "\n")
		//rows.map( row => row.mkString( "\t" ) ).mkString( "\n")
		//"hi"
		*/
	}
	def info(env:Option[Environment]=None) = "UpperTriangularMatrix(" + rows.map( row => row.mkString( ",")).mkString( "\n" ) + ")"

	override def visit(env:Option[Environment]=None):UpperTriangularMatrix = UpperTriangularMatrix(
		rows.map( row => row.map( elem => elem.visit( env ) ) )
	)

	override def eval() = UpperTriangularMatrix(
		rows.map( row => row.map( elem => elem.eval() ) )
	)

	lazy val numCols = rows.size
	lazy val numRows = rows.size

	def *(that:Expr) = UpperTriangularMatrix( rows.map( row => { row.map( elem => Product( elem, that ) ) } ) )
	def tensorProduct(that:Expr) = UpperTriangularMatrix( rows.map( row => { row.map( elem => Product( elem, that ) ) } ) )

	//val lup = Variable( "todo")
	def *(that:DenseMatrix) = this.toDenseMatrix * that.toDenseMatrix

	//val l_lup = Variable( "todo l_lup")
	lazy val lu_lup = ( EyeMatrix( this.numRows ), this )
	lazy val lup_lup = ( EyeMatrix( this.numRows ), this, EyeMatrix( this.numRows ) )
	
	// TODO: Improve implementation
	def solve( rhs:DenseMatrix ):DenseMatrix = {
		var x:ListBuffer[List[Expr]] = ListBuffer.fill(rhs.numRows){ List.fill(rhs.numCols){ Number( 0 ) } } //rhs.rows.map( row => row.to[ListBuffer] ).to[ListBuffer]
		// Backward substitution
		for( i <- this.numRows - 1 to 0 by -1 ) //until this.numRows )
		{
			var xi = rhs.rows( i ).to[ListBuffer]
			for( k <- 0 until xi.size )
			{
				var xik:Expr = xi(k)
				for( j <- i + 1 until this.numRows )
					xik = Sum( xik, Product( Number( -1 ), Product( x(j)(k), rows(i)(j-i) ) ) )

				xi = xi.updated( k, xik )
			}
			x( i ) = xi.map( elem => Fraction( elem, rows(i)(0) ).visit() ).to[List] // ::: x :: Nil// divide by diagonal
		}
		DenseMatrix( x.to[List] )
	}

	lazy val toDenseMatrix:DenseMatrix = {
		var rs:List[List[Expr]] = Nil
		for( i <- 0 until numRows )
		{
			val pre:List[Expr] = List.fill( i ){ Number( 0 ) }
			var row = rows( i )
			row = pre ::: row // :: Nil
			rs = rs :+ row
		}
		DenseMatrix( rs )
	}
	
	lazy val inverse = this.solve( EyeMatrix( this.numRows ).toDenseMatrix ) //Variable( "Todo")

	def transpose = LowerTriangularMatrix( this.rows )
}

/*
* ps is the list of permutation steps
* rowPositions has the final list of rearranged rows
*/
case class RowPermutationMatrixInverse( ps:ListBuffer[Int] ) extends Matrix {
	lazy val numCols = ps.size
	lazy val numRows = ps.size
	val l_lup = Variable( "todo l_lup")
	lazy val lu_lup = ( this, EyeMatrix( numRows ) )
	lazy val lup_lup = ( EyeMatrix( numRows ), EyeMatrix( numRows ), this.inverse )

	def *(that:Expr) = this.toDenseMatrix * that 
	def tensorProduct(that:Expr) = this.toDenseMatrix * that 


	def *(that:DenseMatrix):DenseMatrix = {
		var rows = that.rows
		for( i <- ps.size - 1 to 0 by -1) { 
			val p = ps(i)
				if( p != i )
					rows = rows.updated(i,rows(p)).updated(p,rows(i))
		}
		DenseMatrix( rows )
	}

	def solve( rhs:DenseMatrix ):DenseMatrix = {
		// apply permutation to rhs
		var x = rhs.rows
		for( i <- 0 until ps.size ) //- 1 to 0 by -1 )
			x = x.updated( i, x(ps(i))).updated( ps(i), x(i))
		DenseMatrix( x ) //List( List ( Variable( "todo RowPermutationMatrix solve") ) ) )
	}

	override def toString() = {
		var s = ""
		for( i <- 0 until numRows )
		{
			var r:List[Expr] = List.fill( numCols ){ Number(0) } //todo...
			r = r.updated( rowPositions( i ), Number( 1 ) ) // todo
			s += r.map( elem => elem.toString() ).mkString( "\t") + "\n"
		}
		s
	}
	def info(env:Option[Environment]=None) = "RowPermutationMatrixInverse todo"

	override def visit(env:Option[Environment]=None) = this

	lazy val rowPositions:ListBuffer[Int] = {
		var rp = (0 until ps.size ).to[ListBuffer]
		for( i <- ps.size - 1 to 0 by -1 ) { 
			val p = ps(i)
				if( p != i )
					rp = rp.updated(i,rp(p)).updated(p,rp(i))
		}
		rp
	}

	lazy val toDenseMatrix:DenseMatrix = {
		var rows:List[List[Expr]] = Nil
		for( i <- 0 until numRows )
		{
			var row:List[Expr] = List.fill(numCols){Number(0)}
			row = row.updated(rowPositions(i), Number( 1 ) )
			rows = rows :+ row
		}
		// somehow this creates empty rows!!!!
		DenseMatrix( rows )
	}

/*
	def *(that:LowerTriangularMatrix):LowerTriangularMatrix = {
		that
	}
	*/

// NOT CORRECT
	lazy val inverse:RowPermutationMatrix = {
		//psi = 
		RowPermutationMatrix( ps )
	}

	def transpose = RowPermutationMatrix( ps )
}

/*
* ps is the list of permutation steps
* rowPositions has the final list of rearranged rows
*/
case class RowPermutationMatrix( ps:ListBuffer[Int] ) extends Matrix {

	lazy val numCols = ps.size
	lazy val numRows = ps.size
	lazy val l_lup = Variable( "todo l_lup")
	lazy val lu_lup = ( this, EyeMatrix( numRows ) )
	lazy val lup_lup = ( EyeMatrix( numRows ), EyeMatrix( numRows ), this.inverse )

	def *(that:Expr) = this.toDenseMatrix * that 
	def tensorProduct(that:Expr) = this.toDenseMatrix * that 

	def *(that:DenseMatrix):DenseMatrix = {
		var rows = that.rows
		for( i <- 0 until ps.size ) { 
			val p = ps(i)
				if( p != i )
					rows = rows.updated(i,rows(p)).updated(p,rows(i))
		}
		DenseMatrix( rows )
	}

	def solve( rhs:DenseMatrix ):DenseMatrix = {
		// apply permutation to rhs
		var x = rhs.rows
		for( i <- ps.size - 1 to 0 by -1 )
			x = x.updated( i, x(ps(i))).updated( ps(i), x(i))
		DenseMatrix( x ) //List( List ( Variable( "todo RowPermutationMatrix solve") ) ) )
	}

	override def toString() = {
		//println( "ps: " + ps )
		var s = ""
		for( i <- 0 until numRows )
		{
			var r:List[Expr] = List.fill( numCols ){ Number(0) } //todo...
			r = r.updated( rowPositions( i ), Number( 1 ) ) // todo
			s += r.map( elem => elem.toString() ).mkString( "\t") + "\n"
		}
		s
	}
	def info(env:Option[Environment]=None) = "RowPermutationMatrix todo"

	override def visit(env:Option[Environment]=None) = this

	lazy val rowPositions:ListBuffer[Int] = {
		var rp = (0 until ps.size ).to[ListBuffer]
		for( i <- 0 until ps.size ) { 
			val p = ps(i)
				if( p != i )
					rp = rp.updated(i,rp(p)).updated(p,rp(i))
		}
		rp
	}

	lazy val toDenseMatrix:DenseMatrix = {
		var rows:List[List[Expr]] = Nil
		for( i <- 0 until numRows )
		{
			var row:List[Expr] = List.fill(numCols){Number(0)}
			row = row.updated(rowPositions(i), Number( 1 ) )
			rows = rows :+ row
		}
		// somehow this creates empty rows!!!!
		DenseMatrix( rows )
	}

/*
	def *(that:LowerTriangularMatrix):LowerTriangularMatrix = {
		that
	}
	*/

// NOT CORRECT
	lazy val inverse:RowPermutationMatrixInverse = {
		//psi = 
		RowPermutationMatrixInverse( ps )
	}

	def transpose = RowPermutationMatrixInverse( ps )
}

case class DenseMatrix( rows:List[List[Expr]]) extends Expr with Matrix {
	override def toString() = rows.map( l => l.mkString( "\t" ) ).mkString("\n")
	def info(env:Option[Environment]=None) = "DenseMatrix todo"
	def dotProduct( row:List[Expr], col:List[Expr] ):Expr =
		row.zip( col ).map( { case (a,b) => Product( a, b ) } ).foldLeft(Number(0):Expr)((c,d)=>Sum(c,d)) 
	
	// Matrix multiplication
	def *(that:DenseMatrix):DenseMatrix = {
		val rv = for( row <- this.rows )
			yield for( col <- that.rows.transpose )
				yield dotProduct( row, col ).visit()
		DenseMatrix( rv )
    }

    def *(that:RowPermutationMatrix):DenseMatrix = {
    	var rv = rows
    	for( i <- that.ps.size - 1 to 0 by -1 ) { //0 until ps.size ) { 
			val p = that.ps(i)
				if( p != i )
					rv = rv.updated(i,rv(p)).updated(p,rv(i))
		}
		DenseMatrix( rows )
    }

    def *(that:Expr) = DenseMatrix( rows.map( row => { row.map( elem => Product( elem, that ) ) } ) )
    def tensorProduct(that:Expr) = DenseMatrix( rows.map( row => { row.map( elem => Product( elem, that ) ) } ) )

    def -(that:DenseMatrix):DenseMatrix = {
		val rv = rows.zip( that.rows ).map( {
			case( thisrow, thatrow ) => thisrow.zip( thatrow ).map( {
				case (a,b) => Sum( a, Product( Number( -1 ), b ) )
			} )
		} )
    	DenseMatrix( rv )
    }
	def +(that:DenseMatrix):DenseMatrix = {
		val rv = rows.zip( that.rows ).map( {
			case( thisrow, thatrow ) => thisrow.zip( thatrow ).map( {
				case (a,b) => Sum( a, b )
			} )
		} )
    	DenseMatrix( rv )
    }
    def +(that:Expr):DenseMatrix = {
    	DenseMatrix( rows.map( row => row.map( elem => Sum( elem, that ) ) ) )
    }

    def dotProduct(that:Number):DenseMatrix = {
		val rv = for( row <- this.rows )
			yield for( elem <- row )
				yield Product( elem, that )
		DenseMatrix( rv )
    }
    def dotProduct(that:Variable):DenseMatrix = {
		val rv = for( row <- this.rows )
			yield for( elem <- row )
				yield Product( elem, that )
		DenseMatrix( rv )
    }
	def dotProduct(that:DenseMatrix):DenseMatrix = DenseMatrix(
		this.rows.zip( that.rows ).map( { case( lc, rc) => lc.zip(rc).map( { case (le,re) => Product( le,re ) } ) } )
	)

	override def visit(env:Option[Environment]=None) = DenseMatrix(
		rows.map( row => row.map( elem => elem.visit( env ) ) )
	)

	override def eval() = DenseMatrix(
		rows.map( row => row.map( elem => elem.eval() ) )
	)

	lazy val numRows = rows.size
	lazy val numCols = if (rows.size > 0 ) rows(0).size else 0
	lazy val cols = rows.transpose

	// L*U = P * A, inv(P)*L*U=A, inv(inv(P)*L*U)=inv(A), inv(U)*inv(L)*P=inv(A)
	lazy val inverse = _lup match {
		case (lc,ur,ps) => ( UpperTriangularMatrix( ur ).visit().inverse * LowerTriangularMatrix( lc ).visit().inverse ).visit() * RowPermutationMatrix( ps ).toDenseMatrix
	}

	lazy val lu_lup = _lup match {
		case (lc,ur,ps) => (
			( RowPermutationMatrix( ps ).inverse * LowerTriangularMatrix( lc ).toDenseMatrix ).visit(),
			UpperTriangularMatrix( ur ).visit()
		)
	}

	lazy val lup_lup = _lup match {
		case (lc,ur,ps) => (
			LowerTriangularMatrix( lc ).visit(),
			UpperTriangularMatrix( ur ).visit(),
			RowPermutationMatrix( ps )//.visit() // No visit needed for RowPerm
		)
	}

	// Helper for LU factorization
	lazy val _lup = {
		var lc:List[List[Expr]] = Nil // List of list of expr, cols of L
		var ur:List[List[Expr]] = Nil // List of list of expr, rows of U
		val ps:ListBuffer[Int] = ( 0 until this.numRows ).to[ListBuffer] // P, permutatations
		//println( "ps: " + ps )
		var rs = rows
		var nr = rows
		
		for( i <- 0 until this.numRows )
		{
			// find row with max abs value
			var pi = i // pivot index
			
			var pv = math.abs( rs(0)(0).doubleValue )
			for( p <- i + 1  until this.numRows ) {
				if( math.abs( rs(p-i)(0).doubleValue ) > pv ) {
					pi = p; 
					pv = rs(p-i)(0).doubleValue
				}
			}
			
			ps(i)=pi // these are the indices to use later on, to build the external facing P matrix
			// move elements in rs around
			if( pi != i ){
				//println( "pi: " + pi + "i: " + i + "rs.size: " + rs.size )
				rs = rs.updated(0,rs(pi-i)).updated(pi-i,rs(0))
				nr = nr.updated(i,nr( pi)).updated(pi, nr( i ) )
				// tricky part... if you applied a pivot here, also updated l part that is already built up !!
				for( c <- 0 until lc.size ) {
					val pc = lc(c).updated( i - c, lc(c)(pi-c)).updated( pi - c, lc(c)(i-c))// Permuted Column
					lc = lc.updated( c, pc )
				}					
			}

			var pivot = rs(0)(0) // rs keeps shrinking...

			var cs = rs.transpose
			
			var col = cs( 0 ).takeRight( numRows - i -1 ).map( elem => Fraction( elem, pivot ) )
			lc = lc ::: ( Number( 1 )  :: col ) :: Nil

			ur = ur ::: rs(0).takeRight( numRows - i ) :: Nil
			
			// Now adjust rs to reflect a smaller set of of rows and cols
			rs = nr.takeRight( numRows - 1 - i ).map( row => row.takeRight( numRows - 1 - i ) )


			var l21 = lc.map( col => col.takeRight( numRows - 1 - i ) )//.takeRight( numRows - 1 - i )
			var u12 = ur.map( row => row.takeRight( numRows - 1 - i ) )//.takeRight( numRows - 1 - i )
			// rs <- r1 - L21*U11
			//println( "rs: " + rs + "l21: " + l21 + "u12: " + u12 )
			rs = ( DenseMatrix( rs ) - DenseMatrix( l21.transpose ) * DenseMatrix( u12 ) ).rows
			
			rs = rs.map( row => row.map( elem => elem.visit() ) )
		}

		(lc,ur,ps)
	}

	// Solves X in this *  X = rhs
	// X = inv( this ) * rhs, but that's not how it's calculated
	def solve(rhs:DenseMatrix):DenseMatrix = this._lup match {
		case (lc,ur,ps) =>
			var sol:DenseMatrix = RowPermutationMatrix( ps ) * rhs
			sol = LowerTriangularMatrix( lc ).solve( sol )
			UpperTriangularMatrix( ur ).solve( sol ) 
	}

	override def norm( p:Int ):Expr = p match {
		case 0 => Number( 0 )
		case 1 => Max( rows.transpose.map( row => row.map( elem => Abs( elem ) ).foldLeft( Number( 0 ):Expr )( (a,b) => Sum( a, b ).visit() ) ):_* ).visit() //.foldLeft( Number( 0 ):Expr )( (a,b) => Max( a, b ).visit() )// max column abs sum
		case 2 => ErrorExpr( "Todo DenseMatrix.norm") // requires eigenvalues 
		//case Inf	
	}

	lazy val toDenseMatrix = this
	def transpose = DenseMatrix( rows.transpose ) 
}

// Handler for lu command
case class MatLU(expr:Expr) extends Expr {
	def info(env:Option[Environment]=None) = this.toString()
}

// Handler for inv command
case class MatInv(expr:Expr) extends Expr {
	def info(env:Option[Environment]=None) = this.toString()
	override def visit(env:Option[Environment]=None) = expr.visit( env ) match {	 
        case m:Matrix => m.inverse
        case _ => ErrorExpr( "inv(matrix) can only be applied to a matrix")
    }
}

// Handler for norm command
case class MatNorm(expr:Expr, p:Int = 2) extends Expr {
	def info(env:Option[Environment]=None) = this.toString()
	override def visit(env:Option[Environment]=None) = expr.visit( env ) match {
        case m:Matrix => m.norm(p)
        case _ => ErrorExpr( "norm(matrix) can only be applied to a matrix")
    }
}
