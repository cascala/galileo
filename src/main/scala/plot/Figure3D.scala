package galileo.plot

import galileo.environment.Environment
import galileo.expr._

// A surface sampled on a grid: x(i)(j), y(i)(j), z(i)(j)
case class Surface3D( label: String, x: Vector[ Vector[ Double ] ], y: Vector[ Vector[ Double ] ], z: Vector[ Vector[ Double ] ] ) {
  require( x.size == y.size && y.size == z.size, "Surface3D: x, y and z grids must have the same shape" )
  def rows = x.size
  def cols = if ( x.isEmpty ) 0 else x.head.size
  def points: Iterator[ ( Double, Double, Double ) ] =
    for ( i <- ( 0 until rows ).iterator; j <- ( 0 until cols ).iterator ) yield ( x( i )( j ), y( i )( j ), z( i )( j ) )
}

// A curve in space
case class Curve3D( label: String, points: Vector[ ( Double, Double, Double ) ] )

// The result of a 3D plot command: interactive (Plotly) or a shaded SVG projection in notebooks
case class Figure3D(
    surfaces: List[ Surface3D ],
    curves: List[ Curve3D ],
    title: String = "",
    labels: ( String, String, String ) = ( "x", "y", "z" ),
    equalAspect: Boolean = false ) extends Expr with Statement with Displayable {

  def info( env: Option[ Environment ] = None ) = "Figure3D(" + ( surfaces.map( _.label ) ++ curves.map( _.label ) ).mkString( "," ) + ")"
  override def variables: List[ Variable ] = Nil

  override def toString() = {
    val parts = surfaces.map( s => "surface " + s.label + " (" + s.rows + "x" + s.cols + " grid)" ) ++
      curves.map( c => "curve " + c.label + " (" + c.points.size + " points)" )
    "3D figure: " + parts.mkString( "; " ) + "\n(shown as a 3D graphic in Jupyter notebooks)"
  }

  def mimeBundle: Map[ String, String ] = Map(
    "text/plain" -> toString(),
    "image/svg+xml" -> svg(),
    "application/vnd.plotly.v1+json" -> plotly
  )

  def overlay( other: Figure3D ): Figure3D = Figure3D(
    surfaces ++ other.surfaces,
    curves ++ other.curves,
    if ( title.nonEmpty ) title else other.title,
    labels,
    equalAspect || other.equalAspect )

  private def finite( d: Double ) = !d.isNaN && !d.isInfinite
  private def finite3( p: ( Double, Double, Double ) ) = finite( p._1 ) && finite( p._2 ) && finite( p._3 )

  // Bounds per axis
  lazy val bounds: ( ( Double, Double ), ( Double, Double ), ( Double, Double ) ) = {
    val pts = ( surfaces.iterator.flatMap( _.points ) ++ curves.iterator.flatMap( _.points ) ).filter( finite3 ).toVector
    def b( f: ( ( Double, Double, Double ) ) => Double ) =
      if ( pts.isEmpty ) ( -1.0, 1.0 )
      else { val vs = pts.map( f ); val ( lo, hi ) = ( vs.min, vs.max ); if ( hi - lo < 1e-12 ) ( lo - 1, hi + 1 ) else ( lo, hi ) }
    ( b( _._1 ), b( _._2 ), b( _._3 ) )
  }

  val surfaceScales = Vector( "Viridis", "Reds", "Blues", "Greens", "Purples" )
  val curveColors = Vector( "#dc2626", "#f59e0b", "#2563eb", "#16a34a", "#9333ea", "#0f172a" )

  def plotly: String = {
    import JsonOut._
    def grid( g: Vector[ Vector[ Double ] ] ) = arr( g.map( row => nums( row ) ) )
    val st = surfaces.zipWithIndex.map { case ( s, i ) =>
      obj( "type" -> str( "surface" ), "name" -> str( s.label ), "x" -> grid( s.x ), "y" -> grid( s.y ), "z" -> grid( s.z ),
        "colorscale" -> str( surfaceScales( i % surfaceScales.size ) ), "showscale" -> "false",
        "opacity" -> ( if ( curves.nonEmpty ) "0.85" else "1" ), "showlegend" -> "true" )
    }
    val ct = curves.zipWithIndex.map { case ( c, i ) =>
      obj( "type" -> str( "scatter3d" ), "mode" -> str( "lines" ), "name" -> str( c.label ),
        "x" -> nums( c.points.map( _._1 ) ), "y" -> nums( c.points.map( _._2 ) ), "z" -> nums( c.points.map( _._3 ) ),
        "line" -> obj( "color" -> str( curveColors( i % curveColors.size ) ), "width" -> "6" ) )
    }
    def axis( t: String ) = obj( "title" -> obj( "text" -> str( t ) ), "backgroundcolor" -> str( "#f8fafc" ), "gridcolor" -> str( "#e2e8f0" ), "showbackground" -> "true" )
    val layout = obj(
      "title" -> obj( "text" -> str( title ) ),
      "scene" -> obj(
        "xaxis" -> axis( labels._1 ), "yaxis" -> axis( labels._2 ), "zaxis" -> axis( labels._3 ),
        "aspectmode" -> str( if ( equalAspect ) "data" else "cube" ),
        "camera" -> obj( "eye" -> obj( "x" -> "1.6", "y" -> "-1.6", "z" -> "1.0" ) ) ),
      "showlegend" -> "true",
      "legend" -> obj( "x" -> "0.01", "y" -> "0.99" ),
      "margin" -> obj( "l" -> "0", "r" -> "0", "t" -> ( if ( title.isEmpty ) "10" else "50" ), "b" -> "0" ),
      "width" -> "700", "height" -> "560" )
    obj( "data" -> arr( st ++ ct ), "layout" -> layout )
  }

  // ---- static SVG rendering: shaded, depth-sorted projection ----

  private val az = math.toRadians( 35 )
  private val el = math.toRadians( 25 )

  // Map data to a normalised box: each axis to [-1, 1], or a common scale with equalAspect
  private lazy val ( norm, extent ) = {
    val ( ( x0, x1 ), ( y0, y1 ), ( z0, z1 ) ) = bounds
    val halves = Vector( ( x1 - x0 ) / 2, ( y1 - y0 ) / 2, ( z1 - z0 ) / 2 )
    val centers = Vector( ( x0 + x1 ) / 2, ( y0 + y1 ) / 2, ( z0 + z1 ) / 2 )
    val scales = if ( equalAspect ) Vector.fill( 3 )( halves.max ) else halves
    val f = ( p: ( Double, Double, Double ) ) => ( ( p._1 - centers( 0 ) ) / scales( 0 ), ( p._2 - centers( 1 ) ) / scales( 1 ), ( p._3 - centers( 2 ) ) / scales( 2 ) )
    ( f, ( halves( 0 ) / scales( 0 ), halves( 1 ) / scales( 1 ), halves( 2 ) / scales( 2 ) ) )
  }

  // ( screen right, screen up, depth: larger is farther away )
  private def project( p: ( Double, Double, Double ) ): ( Double, Double, Double ) = {
    val ( x, y, z ) = p
    val x1 = x * math.cos( az ) - y * math.sin( az )
    val y1 = x * math.sin( az ) + y * math.cos( az )
    ( x1, z * math.cos( el ) + y1 * math.sin( el ), y1 * math.cos( el ) - z * math.sin( el ) )
  }

  private def viridis( t0: Double ): ( Double, Double, Double ) = {
    val stops = Vector( ( 68.0, 1.0, 84.0 ), ( 59.0, 82.0, 139.0 ), ( 33.0, 145.0, 140.0 ), ( 94.0, 201.0, 98.0 ), ( 253.0, 231.0, 37.0 ) )
    val t = math.max( 0, math.min( 1, t0 ) ) * ( stops.size - 1 )
    val i = math.min( stops.size - 2, t.toInt )
    val f = t - i
    val ( a, b ) = ( stops( i ), stops( i + 1 ) )
    ( a._1 + ( b._1 - a._1 ) * f, a._2 + ( b._2 - a._2 ) * f, a._3 + ( b._3 - a._3 ) * f )
  }

  private val solidColors = Vector( ( 220.0, 38.0, 38.0 ), ( 37.0, 99.0, 235.0 ), ( 22.0, 163.0, 74.0 ), ( 147.0, 51.0, 234.0 ) )

  private def rgb( c: ( Double, Double, Double ), k: Double ) = {
    def ch( v: Double ) = math.max( 0, math.min( 255, ( v * k ).round.toInt ) )
    f"#${ch( c._1 )}%02x${ch( c._2 )}%02x${ch( c._3 )}%02x"
  }

  def svg( width: Int = 640, height: Int = 520 ): String = {
    val ( ex, ey, ez ) = extent
    val corners = for ( sx <- List( -1, 1 ); sy <- List( -1, 1 ); sz <- List( -1, 1 ) ) yield ( sx * ex, sy * ey, sz * ez )
    val pc = corners.map( project )
    val ( minX, maxX ) = ( pc.map( _._1 ).min, pc.map( _._1 ).max )
    val ( minY, maxY ) = ( pc.map( _._2 ).min, pc.map( _._2 ).max )
    val top = if ( title.isEmpty ) 20.0 else 44.0
    val s = math.min( ( width - 120.0 ) / ( maxX - minX ), ( height - top - 50.0 ) / ( maxY - minY ) )
    val ox = width / 2.0 - s * ( minX + maxX ) / 2
    val oy = top + ( height - top - 30.0 ) / 2 + s * ( minY + maxY ) / 2
    def scr( p: ( Double, Double, Double ) ) = { val ( a, b, _ ) = project( p ); ( ox + s * a, oy - s * b ) }
    def r( d: Double ) = f"$d%.1f"
    def pt( p: ( Double, Double, Double ) ) = { val ( a, b ) = scr( p ); r( a ) + "," + r( b ) }

    val sb = new StringBuilder
    sb ++= s"""<svg xmlns="http://www.w3.org/2000/svg" width="$width" height="$height" viewBox="0 0 $width $height" font-family="sans-serif" font-size="12">"""
    sb ++= s"""<rect width="$width" height="$height" fill="white"/>"""

    // bounding box, drawn first
    val edges = for ( a <- corners; b <- corners if a != b && List( a._1 != b._1, a._2 != b._2, a._3 != b._3 ).count( identity ) == 1 && corners.indexOf( a ) < corners.indexOf( b ) ) yield ( a, b )
    edges.foreach { case ( a, b ) => sb ++= s"""<line x1="${pt( a ).split( "," )( 0 )}" y1="${pt( a ).split( "," )( 1 )}" x2="${pt( b ).split( "," )( 0 )}" y2="${pt( b ).split( "," )( 1 )}" stroke="#cbd5e1"/>""" }

    // primitives: ( depth, svg )
    val prims = scala.collection.mutable.ArrayBuffer[ ( Double, String ) ]()
    val light = { val l = ( -0.4, -0.6, 0.7 ); val n = math.sqrt( l._1 * l._1 + l._2 * l._2 + l._3 * l._3 ); ( l._1 / n, l._2 / n, l._3 / n ) }
    val ( ( _, _ ), ( _, _ ), ( z0, z1 ) ) = bounds
    for ( ( surf, si ) <- surfaces.zipWithIndex; i <- 0 until surf.rows - 1; j <- 0 until surf.cols - 1 ) {
      val raw = List( ( i, j ), ( i + 1, j ), ( i + 1, j + 1 ), ( i, j + 1 ) ).map { case ( a, b ) => ( surf.x( a )( b ), surf.y( a )( b ), surf.z( a )( b ) ) }
      if ( raw.forall( finite3 ) ) {
        val q = raw.map( norm )
        val depth = q.map( p => project( p )._3 ).sum / 4
        // normal from the diagonals, for simple shading
        val ( d1, d2 ) = ( sub( q( 2 ), q( 0 ) ), sub( q( 3 ), q( 1 ) ) )
        val nrm = cross( d1, d2 )
        val len = math.sqrt( dot( nrm, nrm ) )
        val shade = if ( len < 1e-12 ) 0.8 else 0.55 + 0.45 * math.abs( dot( nrm, light ) / len )
        val zMean = raw.map( _._3 ).sum / 4
        val base = if ( si == 0 ) viridis( ( zMean - z0 ) / ( z1 - z0 ) ) else solidColors( ( si - 1 ) % solidColors.size )
        val fill = rgb( base, shade )
        prims += ( ( depth, s"""<polygon points="${q.map( p => pt( p ) ).mkString( " " )}" fill="$fill" stroke="$fill" stroke-width="0.6"/>""" ) )
      }
    }
    for ( ( c, ci ) <- curves.zipWithIndex; Seq( a, b ) <- c.points.sliding( 2 ) if finite3( a ) && finite3( b ) ) {
      val ( na, nb ) = ( norm( a ), norm( b ) )
      val depth = ( project( na )._3 + project( nb )._3 ) / 2 - 0.06 // slightly in front of the surface it lies on
      val color = curveColors( ci % curveColors.size )
      prims += ( ( depth, s"""<line x1="${pt( na ).split( "," )( 0 )}" y1="${pt( na ).split( "," )( 1 )}" x2="${pt( nb ).split( "," )( 0 )}" y2="${pt( nb ).split( "," )( 1 )}" stroke="$color" stroke-width="2.5" stroke-linecap="round"/>""" ) )
    }
    prims.sortBy( -_._1 ).foreach( p => sb ++= p._2 )

    // axis labels: x and y along the bottom edges nearest to the viewer, z on the leftmost vertical edge
    def esc( t: String ) = t.replace( "&", "&amp;" ).replace( "<", "&lt;" ).replace( ">", "&gt;" )
    val ( ( bx0, bx1 ), ( by0, by1 ), ( bz0, bz1 ) ) = bounds
    def fmt( d: Double ) = { val v = if ( math.abs( d ) < 1e-9 ) 0.0 else d; if ( math.abs( v ) >= 1e4 || ( v != 0 && math.abs( v ) < 1e-2 ) ) f"$v%.1e" else f"$v%.2f".reverse.dropWhile( _ == '0' ).reverse.stripSuffix( "." ) }
    def label( p: ( Double, Double, Double ), text: String, anchor: String, dx: Double, dy: Double, bold: Boolean = false ) = {
      val ( a, b ) = scr( p )
      sb ++= s"""<text x="${r( a + dx )}" y="${r( b + dy )}" text-anchor="$anchor" fill="${if ( bold ) "#111827" else "#475569"}"${if ( bold ) " font-weight=\"bold\"" else ""}>${esc( text )}</text>"""
    }
    val bottom = corners.filter( _._3 < 0 )
    val near = bottom.minBy( c => project( c )._3 )
    val ( nx, ny ) = ( near._1, near._2 )
    def side( p: ( Double, Double, Double ) ) = if ( scr( p )._1 < scr( near )._1 ) "end" else "start"
    // x edge: from near corner along x
    val xa = ( -ex, ny, -ez ); val xb = ( ex, ny, -ez )
    label( xa, fmt( bx0 ), side( xa ), if ( side( xa ) == "end" ) -4 else 4, 14 ); label( xb, fmt( bx1 ), side( xb ), if ( side( xb ) == "end" ) -4 else 4, 14 )
    label( ( 0, ny, -ez ), labels._1, "middle", 0, 24, bold = true )
    val ya = ( nx, -ey, -ez ); val yb = ( nx, ey, -ez )
    label( ya, fmt( by0 ), side( ya ), if ( side( ya ) == "end" ) -4 else 4, 14 ); label( yb, fmt( by1 ), side( yb ), if ( side( yb ) == "end" ) -4 else 4, 14 )
    label( ( nx, 0, -ez ), labels._2, "middle", 0, 24, bold = true )
    val left = bottom.minBy( c => scr( c )._1 )
    label( ( left._1, left._2, -ez ), fmt( bz0 ), "end", -8, 0 ); label( ( left._1, left._2, ez ), fmt( bz1 ), "end", -8, 8 )
    label( ( left._1, left._2, 0 ), labels._3, "end", -10, 4, bold = true )
    // legend
    val entries = surfaces.zipWithIndex.map { case ( su, i ) => ( su.label, if ( i == 0 ) rgb( viridis( 0.5 ), 1 ) else rgb( solidColors( ( i - 1 ) % solidColors.size ), 1 ) ) } ++
      curves.zipWithIndex.map { case ( c, i ) => ( c.label, curveColors( i % curveColors.size ) ) }
    for ( ( ( text, color ), i ) <- entries.zipWithIndex ) {
      val y = 16 + i * 18
      sb ++= s"""<rect x="10" y="${y - 6}" width="18" height="10" fill="$color"/><text x="34" y="${y + 3}" fill="#111827">${esc( text )}</text>"""
    }
    if ( title.nonEmpty ) sb ++= s"""<text x="${width / 2}" y="24" text-anchor="middle" font-size="14" fill="#111827">${esc( title )}</text>"""
    sb ++= "</svg>"
    sb.toString
  }

  private def sub( a: ( Double, Double, Double ), b: ( Double, Double, Double ) ) = ( a._1 - b._1, a._2 - b._2, a._3 - b._3 )
  private def dot( a: ( Double, Double, Double ), b: ( Double, Double, Double ) ) = a._1 * b._1 + a._2 * b._2 + a._3 * b._3
  private def cross( a: ( Double, Double, Double ), b: ( Double, Double, Double ) ) =
    ( a._2 * b._3 - a._3 * b._2, a._3 * b._1 - a._1 * b._3, a._1 * b._2 - a._2 * b._1 )
}
