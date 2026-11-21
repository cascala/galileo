package galileo.geometry

import galileo.environment.Environment
import galileo.expr._
import galileo.plot.Displayable
import galileo.selectable.Selectable
import galileo.tensor.{Tensor, TensorIndex, TensorIndexKind}

import TensorIndexKind._

// Index symmetries, used to list each independent component only once
sealed trait Symmetry { def keep( ix: List[ Int ] ): Boolean }
object Symmetry {
  case object NoSymmetry extends Symmetry { def keep( ix: List[ Int ] ) = true }
  // symmetric in the last two indices (Christoffel symbols)
  case object LastPair extends Symmetry { def keep( ix: List[ Int ] ) = ix( ix.size - 2 ) <= ix( ix.size - 1 ) }
  // symmetric rank 2 (metric, Ricci, Einstein)
  case object Pair extends Symmetry { def keep( ix: List[ Int ] ) = ix( 0 ) <= ix( 1 ) }
  // antisymmetric in the last two (R^a_bcd)
  case object AntiLastPair extends Symmetry { def keep( ix: List[ Int ] ) = ix( 2 ) < ix( 3 ) }
  // Riemann-like R_abcd: antisymmetric pairs, symmetric under pair exchange
  case object RiemannLike extends Symmetry {
    def keep( ix: List[ Int ] ) = ix( 0 ) < ix( 1 ) && ix( 2 ) < ix( 3 ) && ( ix( 0 ) < ix( 2 ) || ( ix( 0 ) == ix( 2 ) && ix( 1 ) <= ix( 3 ) ) )
  }
}

// A named set of components (tensor or Christoffel symbols) in a coordinate system,
// displayed as a list of the non-zero independent components.
// In notebooks it is rendered with LaTeX.
case class Components( title: String, symbol: String, latexSymbol: String, kinds: List[ TensorIndexKind ],
    coords: List[ Variable ], components: List[ Expr ], symmetry: Symmetry ) extends Expr with Statement with Selectable with Displayable {

  val dimension = coords.size
  val rank = kinds.size

  def info( env: Option[ Environment ] = None ) = "Components(" + title + ")"

  def location( address: Int ): List[ Int ] =
    ( 0 until rank ).reverse.foldLeft( ( address, List[ Int ]() ) ) { case ( ( rem, acc ), _ ) => ( rem / dimension, ( rem % dimension ) :: acc ) }._2

  def address( ix: List[ Int ] ): Int = ix.foldLeft( 0 )( ( acc, i ) => acc * dimension + i )

  def valueAt( ix: Int* ): Expr = components( address( ix.toList ) )

  def tensor: Tensor = Tensor( kinds.map( k => TensorIndex( k, dimension ) ), components )

  def select( indices: List[ Expr ] ): Expr = {
    val ix = indices.map {
      case Number( v ) => v.toInt
      case _ => return ErrorExpr( "indices must be numbers" )
    }
    if ( ix.size != rank ) return ErrorExpr( symbol + " needs " + rank + " indices" )
    if ( ix.exists( i => i < 0 || i >= dimension ) ) return ErrorExpr( "Index out of bounds" )
    valueAt( ix: _* )
  }

  override def simplify = copy( components = components.map( Tidy( _ ) ) )
  override def expand = copy( components = components.map( _.expand ) )

  private def isZero( e: Expr ) = e match {
    case Number( v ) => v == 0
    case _ => false
  }

  // (indices, value) of the non-zero independent components
  lazy val nonZero: List[ ( List[ Int ], Expr ) ] =
    components.zipWithIndex.map { case ( c, a ) => ( location( a ), c ) }.filter { case ( ix, c ) => symmetry.keep( ix ) && !isZero( c ) }

  private def names = coords.map( _.name.trim )

  private def textIndices( ix: List[ Int ] ): String = {
    val groups = ix.zip( kinds ).foldLeft( List[ ( TensorIndexKind, List[ String ] ) ]() ) { case ( acc, ( i, k ) ) =>
      acc match {
        case ( k2, ns ) :: rest if k2 == k => ( k, ns :+ names( i ) ) :: rest
        case _ => ( k, List( names( i ) ) ) :: acc
      }
    }.reverse
    groups.map { case ( k, ns ) => ( if ( k == Upper ) "^" else "_" ) + ns.mkString( "," ) }.mkString
  }

  private def latexIndices( ix: List[ Int ] ): String =
    ix.zip( kinds ).map { case ( i, k ) =>
      val n = Latex.name( names( i ) )
      if ( k == Upper ) "{}^{" + n + "}" else "{}_{" + n + "}"
    }.mkString

  override def toString(): String = {
    val head = title + " (" + symbol + ", coordinates " + names.mkString( ", " ) + ")"
    if ( nonZero.isEmpty ) head + ": all components are zero"
    else ( head + ", non-zero components:" :: nonZero.map { case ( ix, c ) => "  " + symbol + textIndices( ix ) + " = " + c } ).mkString( "\n" )
  }

  def latex: String = {
    val body =
      if ( nonZero.isEmpty ) latexSymbol + " = 0 \\quad\\text{(all components)}"
      else nonZero.map { case ( ix, c ) => latexSymbol + latexIndices( ix ) + " &= " + Latex( c ) }.mkString( " \\\\\n" )
    "$$\\begin{aligned}\n" + body + "\n\\end{aligned}$$"
  }

  def mimeBundle: Map[ String, String ] = Map(
    "text/plain" -> toString(),
    "text/latex" -> latex,
    "text/markdown" -> ( "**" + title + "** (coordinates " + names.map( n => "$" + Latex.name( n ) + "$" ).mkString( ", " ) + ")\n\n" + latex )
  )
}
