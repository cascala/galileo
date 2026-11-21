package galileo.help

import galileo.environment.Environment
import galileo.expr._
import galileo.plot.Displayable

// Built-in documentation: `help` lists the commands by topic, `help(name)` describes one
object Help {
  case class Entry( name: String, topic: String, usage: String, description: String, example: String = "" )

  val entries: List[ Entry ] = List(
    // Basics
    Entry( "help", "Basics", "help or help(name)", "Lists all commands, or describes one command.", "help(plot)" ),
    Entry( "who", "Basics", "who", "Lists the variables you defined, with their types (whos does the same).", "who" ),
    Entry( "clear", "Basics", "clear", "Removes all variables you defined.", "clear" ),
    Entry( "eval", "Basics", "eval(expr)", "Evaluates an expression numerically, for example a fraction or a constant like pi.", "eval(5/4)" ),
    Entry( "info", "Basics", "info(expr)", "Shows the internal structure of an expression.", "info(x^2+1)" ),
    Entry( "load", "Basics", "load(file)", "Runs the Galileo statements in a file (REPL only).", "load(examples/a.gg)" ),
    // Functions
    Entry( "sin", "Functions", "sin(x), cos(x), tan(x)", "Trigonometric functions.", "sin(pi/6)" ),
    Entry( "cos", "Functions", "cos(x)", "Cosine.", "cos(0)" ),
    Entry( "tan", "Functions", "tan(x)", "Tangent.", "tan(pi/4)" ),
    Entry( "asin", "Functions", "asin(x), acos(x), atan(x)", "Inverse trigonometric functions.", "asin(1)" ),
    Entry( "acos", "Functions", "acos(x)", "Inverse cosine.", "acos(0)" ),
    Entry( "atan", "Functions", "atan(x)", "Inverse tangent.", "atan(1)" ),
    Entry( "exp", "Functions", "exp(x)", "Exponential function.", "exp(1)" ),
    Entry( "log", "Functions", "log(x)", "Natural logarithm.", "log(e)" ),
    Entry( "sqrt", "Functions", "sqrt(x)", "Square root (x^(1/2)); sqrt(-1) is the imaginary unit j.", "sqrt(16)" ),
    // Algebra
    Entry( "simplify", "Algebra", "simplify(expr) or simplify(expr, min)", "Simplifies an expression; with min, searches for the least complex form.", "simplify(sin(y)^2 + cos(y)^2)" ),
    Entry( "expand", "Algebra", "expand(expr)", "Multiplies out products and powers.", "expand((a+b)^2)" ),
    Entry( "factor", "Algebra", "factor(expr)", "Factors an expression.", "factor(a*b + a*c)" ),
    Entry( "complexity", "Algebra", "complexity(expr)", "A measure of how complex an expression is (used by simplify).", "complexity(a*b + a*c)" ),
    Entry( "prove", "Algebra", "prove(a = b), also with <, > or !=", "Tries to prove a relation with Galileo's rewrite rules.", "prove(1/x = x^(-1))" ),
    Entry( "and", "Algebra", "and(a, b), or(a, b), xor(a, b), not(a)", "Logic operations on true, false and numbers.", "and(true, false)" ),
    // Calculus
    Entry( "deriv", "Calculus", "deriv(expr, x)", "Derivative of expr with respect to x.", "deriv(x^3 - 2*x, x)" ),
    Entry( "odesolve", "Calculus", "odesolve([f1 f2], [y1 y2], t, [y1_0 y2_0], t0, t1)", "Integrates dy/dt = f(t, y) numerically from t0 to t1 and returns y(t1).", "odesolve([v, -x], [x v], t, [1 0], 0, pi)" ),
    Entry( "odeplot", "Calculus", "odeplot([f1 f2], [y1 y2], t, [y1_0 y2_0], t0, t1, [X Y])", "Integrates dy/dt = f(t, y) numerically and plots the path given by X, Y (and Z for 3D), expressions in t and the y's.", "odeplot([v, -x], [x v], t, [1 0], 0, 2*pi, [x v])" ),
    // Plotting
    Entry( "plot", "Plotting", "plot(f1, f2, ..., x, from, to) or plot(f, x)", "Plots one or more expressions of x. The range defaults to -10 .. 10.", "plot(sin(x), cos(x), x, 0, 2*pi)" ),
    Entry( "parametric", "Plotting", "parametric(fx, fy, t, from, to)", "Plots the curve (fx(t), fy(t)).", "parametric(cos(3*t), sin(2*t), t, 0, 2*pi)" ),
    Entry( "plot3d", "Plotting", "plot3d(f, x, a, b, y, c, d)", "Plots the surface z = f(x, y) over a <= x <= b, c <= y <= d.", "plot3d(sin(x)*cos(y), x, -pi, pi, y, -pi, pi)" ),
    Entry( "surface", "Plotting", "surface(fx, fy, fz, u, a, b, v, c, d)", "Plots a parametric surface.", "surface(sin(u)*cos(v), sin(u)*sin(v), cos(u), u, 0, pi, v, 0, 2*pi)" ),
    Entry( "parametric3d", "Plotting", "parametric3d(fx, fy, fz, t, from, to)", "Plots a curve in space.", "parametric3d(cos(t), sin(t), t/(2*pi), t, 0, 6*pi)" ),
    Entry( "show", "Plotting", "show(p1, p2, ...)", "Overlays plots: all 2D, or all 3D. Plots are values, so p = plot(...) stores one.", "show(plot(sin(x), x, 0, pi), plot(cos(x), x, 0, pi))" ),
    // Matrices
    Entry( "inv", "Matrices", "inv(A)", "Inverse of a matrix; works with symbolic entries. Matrices are written [a b; c d].", "A = [a b; c d]; inv(A)" ),
    Entry( "lu", "Matrices", "[L, U, P] = lu(A)", "LU factorization with pivoting (also [L, U] = lu(A)).", "A = [1 2; 3 4]; [L, U, P] = lu(A)" ),
    Entry( "transpose", "Matrices", "transpose(A)", "Transpose of a matrix.", "A = [1 2; 3 4]; transpose(A)" ),
    Entry( "norm", "Matrices", "norm(A, 1)", "1-norm of a matrix (largest absolute column sum). The 2-norm is not implemented yet.", "A = [1, -2; 3, 4]; norm(A, 1)" ),
    Entry( "eye", "Matrices", "eye(n) or eye(n, m)", "Identity matrix.", "eye(3)" ),
    Entry( "ones", "Matrices", "ones(n) or ones(n, m)", "Matrix of ones.", "ones(2, 3)" ),
    Entry( "zeros", "Matrices", "zeros(n) or zeros(n, m)", "Matrix of zeros.", "zeros(2)" ),
    Entry( "rand", "Matrices", "rand(n) or rand(n, m)", "Random matrix with entries uniform in [0, 1).", "rand(2)" ),
    Entry( "linspace", "Matrices", "linspace(a, b) or linspace(a, b, n)", "Row vector of n evenly spaced values from a to b.", "linspace(0, 1, 5)" ),
    Entry( "logspace", "Matrices", "logspace(a, b) or logspace(a, b, n)", "Row vector of n values from 10^a to 10^b, evenly spaced on a log scale.", "logspace(0, 2, 3)" ),
    // Differential geometry
    Entry( "metric", "Differential geometry", "metric([x1 x2 ...], [g11 g12 ...; ...]) or metric.generate(name)", "A metric with the given coordinates and components. metric.generate gives a ready-made one: minkowski, euclidean, polar, spherical, two-sphere, three-sphere, torus, hyperbolic, schwarzschild.", "g = metric([r phi], [1 0; 0 r^2])" ),
    Entry( "inducedmetric", "Differential geometry", "inducedmetric([x y z], [u v])", "The metric (first fundamental form) of the surface (x(u,v), y(u,v), z(u,v)).", "inducedmetric([sin(u)*cos(v) sin(u)*sin(v) cos(u)], [u v])" ),
    Entry( "inverse", "Differential geometry", "inverse(g)", "Inverse metric g^ab.", "inverse(metric.generate(polar))" ),
    Entry( "christoffel", "Differential geometry", "christoffel(g), christoffelfirst(g), christoffelsecond(g)", "Christoffel symbols (of the second kind for christoffel).", "christoffel(metric.generate(two-sphere))" ),
    Entry( "riemannsecond", "Differential geometry", "riemannsecond(g), riemannfirst(g)", "Riemann curvature tensor R^a_bcd (second) or R_abcd (first).", "riemannsecond(metric.generate(two-sphere))" ),
    Entry( "riccitensor", "Differential geometry", "riccitensor(g), ricciscalar(g)", "Ricci tensor and Ricci scalar.", "ricciscalar(metric.generate(two-sphere))" ),
    Entry( "einsteintensor", "Differential geometry", "einsteintensor(g), einsteinscalar(g)", "Einstein tensor G_ab.", "einsteintensor(metric.generate(schwarzschild))" ),
    Entry( "weyltensor", "Differential geometry", "weyltensor(g)", "Weyl conformal tensor. Also schoutentensor, cottontensor and lanczostensor.", "weyltensor(metric.generate(schwarzschild))" ),
    Entry( "gausscurvature", "Differential geometry", "gausscurvature(g)", "Gaussian curvature of a 2D metric.", "gausscurvature(metric.generate(two-sphere))" ),
    Entry( "kretschmann", "Differential geometry", "kretschmann(g)", "Kretschmann scalar R_abcd R^abcd.", "kretschmann(metric.generate(schwarzschild))" ),
    Entry( "geodesics", "Differential geometry", "geodesics(g)", "The geodesic equations of a metric.", "geodesics(metric.generate(two-sphere))" ),
    Entry( "geodesic", "Differential geometry", "geodesic(g, x0, v0, s1) or geodesic(g, x0, v0, s1, [X Y Z])", "Integrates the geodesic from position x0 with velocity v0 up to parameter s1 and plots it, optionally mapped through X, Y (, Z). Several rows in v0 give several geodesics.", "geodesic(metric.generate(two-sphere), [pi/2 0], [1 1], 6, [sin(theta)*cos(phi) sin(theta)*sin(phi) cos(theta)])" )
  )

  // Alternative names that share an entry
  private val aliases = Map(
    "whos" -> "who", "or" -> "and", "xor" -> "and", "not" -> "and",
    "christoffelfirst" -> "christoffel", "christoffelsecond" -> "christoffel",
    "riemannfirst" -> "riemannsecond", "ricciscalar" -> "riccitensor", "einsteinscalar" -> "einsteintensor",
    "schoutentensor" -> "weyltensor", "cottontensor" -> "weyltensor", "lanczostensor" -> "weyltensor",
    "metric.generate" -> "metric" )

  def entry( name: String ): Option[ Entry ] = {
    val key = aliases.getOrElse( name.trim, name.trim )
    entries.find( _.name == key )
  }

  def topics: List[ ( String, List[ Entry ] ) ] = {
    val order = entries.map( _.topic ).distinct
    order.map( t => t -> entries.filter( _.topic == t ) )
  }

  def plain( e: Entry ): String =
    e.usage + "\n  " + e.description + ( if ( e.example.nonEmpty ) "\n  Example: " + e.example else "" )

  def markdown( e: Entry ): String =
    "**`" + e.usage + "`**\n\n" + e.description + ( if ( e.example.nonEmpty ) "\n\nExample: `" + e.example + "`" else "" )

  def overviewPlain: String =
    "Galileo commands (help(name) for details):\n" +
      topics.map { case ( t, es ) => "  " + t + ": " + es.map( _.name ).mkString( ", " ) }.mkString( "\n" ) +
      "\nOperators: + - * / ^, comparisons with prove(...). Constants: pi, e, i (or j). End a line with ; to hide its result in a notebook."

  def overviewMarkdown: String =
    "**Galileo commands**. Use `help(name)` for details, or Shift+Tab on a name in a notebook.\n\n" +
      topics.map { case ( t, es ) => "* **" + t + ":** " + es.map( e => "`" + e.name + "`" ).mkString( ", " ) }.mkString( "\n" ) +
      "\n\nOperators: `+ - * / ^`. Constants: `pi`, `e`, `i` (or `j`). End a line with `;` to hide its result.\n\n" +
      "Full reference: [language.md](https://github.com/cascala/galileo/blob/master/docs/language.md)"

  def apply( name: Option[ String ] ): Expr = name match {
    case None => HelpText( overviewPlain, overviewMarkdown )
    case Some( n ) => entry( n ) match {
      case Some( e ) => HelpText( plain( e ), markdown( e ) )
      case None =>
        val close = entries.map( _.name ).filter( c => c.startsWith( n.take( 2 ) ) )
        ErrorExpr( "help: no command called '" + n + "'" + ( if ( close.nonEmpty ) ". Did you mean: " + close.mkString( ", " ) + "?" else ". Type help for a list." ) )
    }
  }
}

// The result of help: plain text in the REPL, Markdown in notebooks
case class HelpText( text: String, markdown: String ) extends Expr with Statement with Displayable {
  def info( env: Option[ Environment ] = None ) = "HelpText"
  override def variables: List[ Variable ] = Nil
  override def toString() = text
  def mimeBundle: Map[ String, String ] = Map( "text/plain" -> text, "text/markdown" -> markdown )
}

// help / help(name) as parsed
case class HelpRequest( name: Option[ String ] ) extends Expr with Statement {
  def info( env: Option[ Environment ] = None ) = "HelpRequest(" + name.getOrElse( "" ) + ")"
  override def variables: List[ Variable ] = Nil
  override def visit( env: Option[ Environment ] = None ): Expr = Help( name )
}
