package galileo.plot

import galileo.environment.Environment
import galileo.expr._

case class Series( label: String, points: Vector[ ( Double, Double ) ] ) {
  def finite: Vector[ ( Double, Double ) ] = points.filter { case ( x, y ) => isFinite( x ) && isFinite( y ) }
  private def isFinite( d: Double ) = !d.isNaN && !d.isInfinite
}

// The result of a 2D plot command: interactive (Plotly) or SVG in notebooks, ASCII art in the REPL
case class Figure( series: List[ Series ], xLabel: String, title: String, yLabel: String = "", equalAspect: Boolean = false )
    extends Expr with Statement with Displayable {
  def info( env: Option[ Environment ] = None ) = "Figure(" + series.map( _.label ).mkString( "," ) + ")"
  override def variables: List[ Variable ] = Nil

  override def toString() = ascii()

  def mimeBundle: Map[ String, String ] = Map(
    "text/plain" -> ascii(),
    "image/svg+xml" -> svg(),
    "application/vnd.plotly.v1+json" -> plotly
  )

  // Combine with another figure (labels and title of the first win when set)
  def overlay( other: Figure ): Figure = Figure(
    series ++ other.series,
    if ( xLabel.nonEmpty ) xLabel else other.xLabel,
    if ( title.nonEmpty ) title else other.title,
    if ( yLabel.nonEmpty ) yLabel else other.yLabel,
    equalAspect || other.equalAspect )

  // Points of a series split into drawable runs: breaks at NaN/infinity and at poles that cross the visible range
  def runs( s: Series ): List[ Vector[ ( Double, Double ) ] ] = {
    val out = List.newBuilder[ Vector[ ( Double, Double ) ] ]
    var cur = Vector.newBuilder[ ( Double, Double ) ]
    var n = 0
    var prev: Option[ Double ] = None
    def flush(): Unit = { if ( n > 0 ) out += cur.result(); cur = Vector.newBuilder; n = 0 }
    for ( ( x, y ) <- s.points ) {
      val ok = !x.isNaN && !y.isNaN && !x.isInfinite && !y.isInfinite
      val jump = prev.exists( p => ok && ( ( p > yMax && y < yMin ) || ( p < yMin && y > yMax ) ) )
      if ( !ok || jump ) flush()
      if ( ok ) { cur += ( ( x, y ) ); n += 1; prev = Some( y ) } else prev = None
    }
    flush()
    out.result()
  }

  // Plotly figure specification (rendered interactively by JupyterLab with the plotly extension)
  def plotly: String = {
    import JsonOut._
    val traces = series.zipWithIndex.map { case ( s, i ) =>
      val rs = runs( s )
      val xs = rs.flatMap( r => r.map( _._1 ) :+ Double.NaN )
      val ys = rs.flatMap( r => r.map( _._2 ) :+ Double.NaN )
      obj( "type" -> str( "scatter" ), "mode" -> str( "lines" ), "name" -> str( s.label ),
        "x" -> nums( xs ), "y" -> nums( ys ), "line" -> obj( "color" -> str( palette( i % palette.size ) ), "width" -> "2" ) )
    }
    val yaxis = List( "range" -> nums( List( yMin, yMax ) ), "zeroline" -> "true", "title" -> obj( "text" -> str( yLabel ) ) ) ++
      ( if ( equalAspect ) List( "scaleanchor" -> str( "x" ), "scaleratio" -> "1" ) else Nil )
    val layout = obj(
      "title" -> obj( "text" -> str( title ) ),
      "xaxis" -> obj( "title" -> obj( "text" -> str( xLabel ) ), "zeroline" -> "true" ),
      "yaxis" -> obj( yaxis: _* ),
      "showlegend" -> "true",
      "legend" -> obj( "x" -> "0.01", "y" -> "0.99", "bgcolor" -> str( "rgba(255,255,255,0.8)" ) ),
      "margin" -> obj( "l" -> "60", "r" -> "20", "t" -> ( if ( title.isEmpty ) "20" else "50" ), "b" -> "50" ),
      "width" -> "700", "height" -> "440",
      "template" -> obj( "layout" -> obj( "plot_bgcolor" -> str( "white" ), "xaxis" -> obj( "gridcolor" -> str( "#e5e7eb" ) ), "yaxis" -> obj( "gridcolor" -> str( "#e5e7eb" ) ) ) )
    )
    obj( "data" -> arr( traces ), "layout" -> layout )
  }

  // Colors for successive series
  val palette = Vector( "#2563eb", "#dc2626", "#16a34a", "#9333ea", "#ea580c", "#0891b2", "#be185d", "#4d7c0f" )

  // Data bounds, ignoring poles: when a few points blow up (tan, 1/x), clip to the bulk of the data
  lazy val ( xMin, xMax, yMin, yMax ) = {
    val pts = series.flatMap( _.finite )
    if ( pts.isEmpty ) ( -1.0, 1.0, -1.0, 1.0 )
    else {
      val xs = pts.map( _._1 )
      val ys = pts.map( _._2 ).sorted
      def q( f: Double ) = ys( math.min( ys.size - 1, ( f * ( ys.size - 1 ) ).round.toInt ) )
      val ( lo, hi ) = ( ys.head, ys.last )
      val ( q1, q9 ) = ( q( 0.02 ), q( 0.98 ) )
      val core = q9 - q1
      val ( y0, y1 ) = if ( core > 0 && hi - lo > 20 * core ) ( q1 - core * 0.5, q9 + core * 0.5 ) else ( lo, hi )
      val ( ya, yb ) = if ( y1 - y0 < 1e-12 ) ( y0 - 1, y1 + 1 ) else ( y0, y1 )
      val pad = ( yb - ya ) * 0.05
      val ( xa, xb ) = ( xs.min, xs.max )
      val ( xa1, xb1 ) = if ( xb - xa < 1e-12 ) ( xa - 1, xb + 1 ) else ( xa, xb )
      if ( !equalAspect ) ( xa1, xb1, ya - pad, yb + pad )
      else {
        // same units per pixel on both axes (plot area of the default SVG is 560 x 336)
        val ( ratio, xp, yp ) = ( 336.0 / 560.0, ( xb1 - xa1 ) * 0.05, pad )
        val ( w, h ) = ( xb1 - xa1 + 2 * xp, yb - ya + 2 * yp )
        val ( cx, cy ) = ( ( xa1 + xb1 ) / 2, ( ya + yb ) / 2 )
        if ( h / w > ratio ) { val w2 = h / ratio; ( cx - w2 / 2, cx + w2 / 2, cy - h / 2, cy + h / 2 ) }
        else { val h2 = w * ratio; ( cx - w / 2, cx + w / 2, cy - h2 / 2, cy + h2 / 2 ) }
      }
    }
  }

  // "Nice" tick positions (1, 2, 5 x 10^k)
  def ticks( lo: Double, hi: Double, target: Int = 6 ): List[ Double ] = {
    val raw = ( hi - lo ) / target
    val mag = math.pow( 10, math.floor( math.log10( raw ) ) )
    val step = List( 1.0, 2.0, 5.0, 10.0 ).map( _ * mag ).find( _ >= raw ).getOrElse( 10 * mag )
    val first = math.ceil( lo / step ) * step
    Iterator.iterate( first )( _ + step ).takeWhile( _ <= hi + step * 1e-9 ).map( t => if ( math.abs( t ) < step * 1e-9 ) 0.0 else t ).toList
  }

  def fmt( d: Double ): String = {
    if ( d == 0 ) "0"
    else if ( math.abs( d ) >= 1e5 || math.abs( d ) < 1e-3 ) f"$d%.2e"
    else {
      val s = f"$d%.4f".reverse.dropWhile( _ == '0' ).reverse
      if ( s.endsWith( "." ) ) s.dropRight( 1 ) else s
    }
  }

  private def esc( s: String ) = s.replace( "&", "&amp;" ).replace( "<", "&lt;" ).replace( ">", "&gt;" ).replace( "\"", "&quot;" )

  def svg( width: Int = 640, height: Int = 400 ): String = {
    val ( ml, mr, mt, mb ) = ( 60.0, 20.0, if ( title.isEmpty ) 20.0 else 40.0, 44.0 )
    val ( pw, ph ) = ( width - ml - mr, height - mt - mb )
    def sx( x: Double ) = ml + ( x - xMin ) / ( xMax - xMin ) * pw
    def sy( y: Double ) = mt + ( 1 - ( y - yMin ) / ( yMax - yMin ) ) * ph
    def r( d: Double ) = f"$d%.2f"
    val sb = new StringBuilder
    sb ++= s"""<svg xmlns="http://www.w3.org/2000/svg" width="$width" height="$height" viewBox="0 0 $width $height" font-family="sans-serif" font-size="12">"""
    sb ++= s"""<rect width="$width" height="$height" fill="white"/>"""
    sb ++= s"""<clipPath id="plotarea"><rect x="${r( ml )}" y="${r( mt )}" width="${r( pw )}" height="${r( ph )}"/></clipPath>"""
    // grid and ticks
    for ( t <- ticks( xMin, xMax ) ) {
      val x = r( sx( t ) )
      sb ++= s"""<line x1="$x" y1="${r( mt )}" x2="$x" y2="${r( mt + ph )}" stroke="#e5e7eb"/>"""
      sb ++= s"""<text x="$x" y="${r( mt + ph + 16 )}" text-anchor="middle" fill="#374151">${fmt( t )}</text>"""
    }
    for ( t <- ticks( yMin, yMax ) ) {
      val y = r( sy( t ) )
      sb ++= s"""<line x1="${r( ml )}" y1="$y" x2="${r( ml + pw )}" y2="$y" stroke="#e5e7eb"/>"""
      sb ++= s"""<text x="${r( ml - 6 )}" y="$y" dy="4" text-anchor="end" fill="#374151">${fmt( t )}</text>"""
    }
    // axes through the origin when visible
    if ( xMin < 0 && xMax > 0 ) sb ++= s"""<line x1="${r( sx( 0 ) )}" y1="${r( mt )}" x2="${r( sx( 0 ) )}" y2="${r( mt + ph )}" stroke="#9ca3af"/>"""
    if ( yMin < 0 && yMax > 0 ) sb ++= s"""<line x1="${r( ml )}" y1="${r( sy( 0 ) )}" x2="${r( ml + pw )}" y2="${r( sy( 0 ) )}" stroke="#9ca3af"/>"""
    sb ++= s"""<rect x="${r( ml )}" y="${r( mt )}" width="${r( pw )}" height="${r( ph )}" fill="none" stroke="#6b7280"/>"""
    // curves, broken at NaN / infinity and at jumps that leave the visible range (poles)
    sb ++= """<g clip-path="url(#plotarea)" fill="none" stroke-width="2" stroke-linejoin="round">"""
    for ( ( s, i ) <- series.zipWithIndex ) {
      val color = palette( i % palette.size )
      val d = new StringBuilder
      var pen = false
      var prev: Option[ Double ] = None
      for ( ( x, y ) <- s.points ) {
        val ok = !x.isNaN && !y.isNaN && !x.isInfinite && !y.isInfinite
        val jump = prev.exists( p => ok && ( ( p > yMax && y < yMin ) || ( p < yMin && y > yMax ) ) )
        if ( !ok || jump ) pen = false
        if ( ok ) {
          val yy = math.max( yMin - ( yMax - yMin ), math.min( yMax + ( yMax - yMin ), y ) )
          d ++= ( if ( pen ) "L" else "M" ) + r( sx( x ) ) + "," + r( sy( yy ) )
          pen = true
          prev = Some( y )
        } else prev = None
      }
      sb ++= s"""<path d="$d" stroke="$color"/>"""
    }
    sb ++= "</g>"
    // labels
    if ( title.nonEmpty ) sb ++= s"""<text x="${r( ml + pw / 2 )}" y="24" text-anchor="middle" font-size="14" fill="#111827">${esc( title )}</text>"""
    if ( xLabel.nonEmpty ) sb ++= s"""<text x="${r( ml + pw / 2 )}" y="${r( height - 8.0 )}" text-anchor="middle" fill="#111827">${esc( xLabel )}</text>"""
    if ( yLabel.nonEmpty ) sb ++= s"""<text x="14" y="${r( mt + ph / 2 )}" text-anchor="middle" fill="#111827" transform="rotate(-90 14 ${r( mt + ph / 2 )})">${esc( yLabel )}</text>"""
    // legend, on a light backdrop so curves do not run through the labels
    if ( series.nonEmpty ) {
      val w = 46 + 7 * series.map( _.label.length ).max
      sb ++= s"""<rect x="${r( ml + 4 )}" y="${r( mt + 4 )}" width="$w" height="${18 * series.size + 2}" fill="white" fill-opacity="0.85" stroke="#e5e7eb"/>"""
    }
    for ( ( s, i ) <- series.zipWithIndex ) {
      val y = mt + 14 + i * 18
      val color = palette( i % palette.size )
      sb ++= s"""<line x1="${r( ml + 10 )}" y1="${r( y )}" x2="${r( ml + 30 )}" y2="${r( y )}" stroke="$color" stroke-width="2"/>"""
      sb ++= s"""<text x="${r( ml + 36 )}" y="${r( y )}" dy="4" fill="#111827" paint-order="stroke" stroke="white" stroke-width="3">${esc( s.label )}</text>"""
    }
    sb ++= "</svg>"
    sb.toString
  }

  // A small text rendering for terminals
  def ascii( width: Int = 64, height: Int = 18 ): String = {
    val marks = "*+ox#@%&"
    val grid = Array.fill( height, width )( ' ' )
    def col( x: Double ) = ( ( x - xMin ) / ( xMax - xMin ) * ( width - 1 ) ).round.toInt
    def row( y: Double ) = ( ( 1 - ( y - yMin ) / ( yMax - yMin ) ) * ( height - 1 ) ).round.toInt
    if ( yMin < 0 && yMax > 0 ) { val r0 = row( 0 ); for ( c <- 0 until width ) grid( r0 )( c ) = '-' }
    if ( xMin < 0 && xMax > 0 ) { val c0 = col( 0 ); for ( r0 <- 0 until height ) grid( r0 )( c0 ) = if ( grid( r0 )( c0 ) == '-' ) '+' else '|' }
    for ( ( s, i ) <- series.zipWithIndex; ( x, y ) <- s.finite ) {
      val ( c, r0 ) = ( col( x ), row( y ) )
      if ( c >= 0 && c < width && r0 >= 0 && r0 < height ) grid( r0 )( c ) = marks( i % marks.length )
    }
    val yl = fmt( yMax ).length max fmt( yMin ).length
    val lines = grid.zipWithIndex.map { case ( line, i ) =>
      val label = if ( i == 0 ) fmt( yMax ) else if ( i == height - 1 ) fmt( yMin ) else ""
      ( " " * ( yl - label.length ) ) + label + " |" + line.mkString
    }
    val axis = ( " " * yl ) + " +" + ( "-" * width )
    val xl = fmt( xMin ); val xr = fmt( xMax )
    val xs = ( " " * ( yl + 2 ) ) + xl + ( " " * math.max( 1, width - xl.length - xr.length ) ) + xr
    val legend = series.zipWithIndex.map { case ( s, i ) => "  " + marks( i % marks.length ) + " " + s.label }
    ( lines.toList ++ List( axis, xs ) ++ legend ).mkString( "\n" )
  }
}
