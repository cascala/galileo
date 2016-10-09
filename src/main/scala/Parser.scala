package galileo.parser

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.ImplicitConversions // really useful...
import java.io.IOException

import galileo.expr._
import galileo.linalg._
import galileo.logic._
import galileo.proof.Proof
import galileo.manipulate.{Expand,Factor,Simplify}
import galileo.solve.Solve
import galileo.trigonometry._
import galileo.tensor._
import galileo.rand.Rand

class Parser extends builtinParser with exprParser { 
	// lexer can be used to reserve keywords
  def program = rep1sep(statement, ";") <~ opt(";")
  def statement:Parser[Expr] =  builtin | prove | assignment | expression // | selector // | expression // | selector
  def builtin:Parser[Expr] = comment | whos | who | clear | ls | cd | pwd | load | exit
  def prove:Parser[Expr] = "prove" ~ "(" ~> expression ~ ( "=" | ">" | "<" | "!=" ) ~ expression <~ ")" ^^ { case l~o~r => Proof( l, o, r ) }
  // Assignment and array assignment (AssignmentN)
  def assignment:Parser[Expr] = 
    ( "[" ~> ident <~ "," ) ~ (ident <~ "," ) ~ ( ident <~ "]" ) ~ ( "=" ~> expression ) ^^ { case a~b~c~e => Assignment3( a, b, c, e ) } |
    ( "[" ~> ident <~ "," ) ~ ( ident <~ "]" ) ~ ( "=" ~> expression ) ^^ { case a~b~e => Assignment2( a, b, e ) } |
    ident ~ ( "=" ~> expression ) ^^ { case name~e => Assignment( name, e ) }
  
/*
override def selector:Parser[Expr] = 
    expression ~ ( "[" ~> expression <~ "]" ) ^^ { case selectee~index0 => Selector( selectee, index0 ) }
*/
  override def failure(msg: String) = "" ~> super.failure(msg)
  // VERY IMPORTANT
  // NEEDS parseAll rather than parse
  def parse(str:String):ParseResult[List[Expr]] = parseAll(program, str)
}

trait exprParser extends logicParser with functionParser with matrixParser with tensorParser {
  val expression:Parser[Expr] = chainl1( term, "+" ^^^ Sum2 | "-" ^^^ Sub | "||" ^^^ Or2 ) 
  def term:Parser[Expr] = chainl1( power, "*" ^^^ Product2 | "/" ^^^ Div | "&&" ^^^ And2 ) 
  def power:Parser[Expr] = chainl1( factor, "^" ^^^ Power ) 
  def factor:Parser[Expr] = selector | mats | tensor | function | deriv | boolOps |
    rowVector |
    floatingPointNumber ^^ NumD | 
    bool | 
    variableE |
    "(" ~> expression <~ ")" | unaryMinus | "" ~> failure( "factor expected")
  def unaryMinus:Parser[Expr] = "-" ~> term ^^ { e:Expr => Product( Number( -1 ), e ) }
  // for matrix selection, e.g. A[0] or A[0,1]
  def selector:Parser[Expr] = 
    ident ~ ( "[" ~> factor <~ "," ) ~ factor <~ "]" ^^ { case selectee~index0~index1 => Selector( Variable( selectee ), index0, index1 ) } |
    ident ~ ( "[" ~> factor <~ "]" ) ^^ { case selectee~index0 => Selector( Variable( selectee ), index0 ) } |
    mats ~ ( "[" ~> factor <~ "," ) ~ factor <~ "]" ^^ { case selectee~index0~index1 => Selector( selectee , index0, index1 ) } |
    mats ~ ( "[" ~> factor <~ "]" ) ^^ { case selectee~index0 => Selector( selectee, index0 ) } |
    tensor ~ ( "[" ~> factor <~ "," ) ~ factor <~ "]" ^^ { case selectee~index0~index1 => Selector( selectee, index0, index1 ) } |
    tensor ~ ( "[" ~> factor <~ "]" ) ^^ { case selectee~index0 => Selector( selectee, index0 ) } 
  // An expression that is not a matrix or vector
  val m_expression = chainl1( m_term, "+" ^^^ Sum2 | "-" ^^^ Sub | "||" ^^^ Or2 ) 
  def m_term:Parser[Expr] = chainl1( m_power, "*" ^^^ Product2 | "/" ^^^ Div | "&&" ^^^ And2 ) 
  def m_power:Parser[Expr] = chainl1( m_factor, "^" ^^^ Power ) 
  def m_factor:Parser[Expr] = function | deriv | boolOps |
    floatingPointNumber ^^ NumD | 
    bool | 
    variableE | //bool | //rowVector |
    "(" ~> m_expression <~ ")" | m_unaryMinus | "" ~> failure( "factor expected")
  def m_unaryMinus:Parser[Expr] = "-" ~> m_term ^^ { e:Expr => Product( Number( -1 ), e ) }

  def deriv:Parser[Expr] = ( "deriv" ~> "(" ~> expression ) ~ ( "," ~> variableV <~ ")" ) ^^ { case y~x => Derivative( y, x ) }

  val Sub = ( a:Expr, b:Expr ) => Sum( a, Product( Number( -1 ), b ) )
  val Div = ( a:Expr, b:Expr ) => Fraction( a, b ) 
  val Product2 = (a:Expr, b:Expr ) => Product( a, b )
  val Sum2 = (a:Expr, b:Expr ) => Sum( a, b )
  val Or2 = (a:Expr,b:Expr) => BoolOrSc( a, b ) // sc for short-circuit evalualation
  val And2 = (a:Expr,b:Expr) => BoolAndSc( a, b )
}

trait tensorParser extends JavaTokenParsers with ImplicitConversions {
  val expression:Parser[Expr]
  def tensor = metric | christoffel | tensors
  def metric:Parser[Metric] = "metric.generate(" ~> metrictemplate <~ ")" ^^ { 
    case "two-sphere" => Metric.twoSphere( Variable( "r" ) )
    case "three-sphere" => Metric.threeSphere( Variable( "r" ) )
    case "schwarzschild" => Metric.schwarzschild( Variable( "r") )
  } 
  def metrictemplate:Parser[String] = "two-sphere" | "three-sphere" | "schwarzschild"
  def christoffel = // these are not tensors - small technicality
    "christoffelfirst(" ~> expression <~ ")" ^^ ChristoffelFirstU |
    "christoffelsecond(" ~> expression <~ ")" ^^ ChristoffelSecondU

  def tensors = 
    "riemannfirst" ~> "(" ~> expression <~ ")" ^^ RiemannFirstU |
    "riemannsecond" ~> "(" ~> expression <~ ")" ^^ RiemannSecondU |
    "einsteintensor" ~> "(" ~> expression <~ ")" ^^ EinsteinTensorU |
    "einsteinscalar" ~> "(" ~> expression <~ ")" ^^ EinsteinScalarU |
    "riccitensor" ~> "(" ~> expression <~ ")" ^^ RicciTensorU |
    "ricciscalar" ~> "(" ~> expression <~ ")" ^^ RicciScalarU /*|
    "schoutentensor" ~> "(" ~> expression <~ ")" ^^ SchoutenTensorU |
    "weyltensor" ~> "(" ~> expression <~ ")" ^^ WeylTensorU |
    "cottontensor" ~> "(" ~> expression <~ ")" ^^ CottonTensorU |
    "lanczostensor" ~> "(" ~> expression <~ ")" ^^ LanczosTensorU  
    */  
}

  //def tensor = "tensor" ~> "(" ~> expr ~ "," ~ expr ~ "," ~ expr <~ ")" ^^ { case l~_~u~_~d => TensorU( l, u, d ) } 

  //def tensor = christof // "hello" ^^^{ Number( 1 ) } //christof
 // def christof:Parser[Expr] = //"christof" ~> "(" ~> expression ~ ( "," ~> vector <~ ")" ) ^^ { case metric~coords => Christoffel( metric, coords, 1 ) } |
  //  "christof" ~> "(" ~> expression ~ ( "," ~> expression <~ ")" ) ^^ { case metric~coords => Christoffel( metric, coords, 1 ) }
  //def vector:Parser[List[Expr]] = "[" ~> rep1sep( expression, " " ) <~ "]" ^^ { case es => es.to[List] }

trait matrixParser extends JavaTokenParsers with ImplicitConversions {
  val expression:Parser[Expr]
  // m_expression is an expression that is not a matrix or vector itself
  // m_expression can be elements of a matrix
  val m_expression:Parser[Expr]
  // content valid inside a matrix  
  //def matrixContent:Parser[Expr] = m_expression //unaryMinusMatrixTerm | matrixTerm | // | 
  def rand:Parser[Expr] = 
    "rand" ~ "(" ~> m_expression ~ "," ~ m_expression <~ ")" ^^ { case nr~_~nc => Rand( nr,nc ) } |
    "rand" ~ "(" ~> m_expression <~ ")" ^^ { case e => Rand( e ) }
  
  def mats:Parser[Expr] = matsolve | matlu | matinv | matnorm | matrix | matrixT
  def matsolve:Parser[Expr] = ( matrix | variableE ) ~ ( '\\' ~> ( matrix | variableE ) ) ^^ Solve
  def matlu:Parser[Expr] = "lu" ~> "(" ~> ( matrix | variableE )  <~ ")" ^^ MatLU
  def matinv:Parser[Expr] = "inv" ~> "(" ~> ( m_expression )  <~ ")" ^^ MatInv
  def matnorm:Parser[Expr] = 
    ( "norm" ~> "(" ~> expression <~ "," ) ~ ( posNumber <~ ")" ) ^^ { case m~p => MatNorm( m, p ) } |
    "norm" ~> "(" ~> expression <~ ")" ^^ { case m => MatNorm( m ) }

  def posNumber:Parser[Integer] = wholeNumber ^^ { s:String => { val r = s.toInt; require( r >= 0 ); r } } 
  def ones:Parser[Expr] = 
    ( "ones" ~> "(" ~> m_expression ) ~ ( "," ~> m_expression <~ ")" ) ^^ { case n~m => OnesMatrixU( n, m ) } |
    "ones" ~> "(" ~> m_expression <~ ")" ^^ { case e => OnesMatrixU( e, e ) } |
    "ones" ^^ { _ => Number( 1 ) }
  def zeros:Parser[Expr] = 
    ( "zeros" ~> "(" ~> m_expression ) ~ ( "," ~> m_expression <~ ")" ) ^^ { case n~m => OnesMatrixU( n, m, Number( 0 ) ) } |
    "zeros" ~> "(" ~> m_expression <~ ")" ^^ { case e => OnesMatrixU( e, e, Number( 0 ) ) } |
    "zeros" ^^ { _ => Number( 0 ) }

  def eye:Parser[Expr] = 
    ( "eye" ~> "(" ~> m_expression ) ~ ( "," ~> m_expression <~ ")" ) ^^ { case n~m => EyeMatrixU( n, m ) } |
    "eye" ~> "(" ~> m_expression <~ ")" ^^ { case e => EyeMatrixU( e, e ) } |
    "eye" ^^ { _ => Number( 1 ) }

  // TODO: true and false are not valid variable names!!!
  def variableV:Parser[Variable] = ident ^^ Variable
  def variableE:Parser[Expr] = ident ^^ Variable
  //def vectorT:Parser[Expr] = vector <~ ''' ^^ { case Vector( l ) => Matrix( l.map( e=> List(e) ) ) }//VecT //| matrix <~ ''' MatT
  def matrixT:Parser[Expr] = matrix <~ ''' ^^ { case DenseMatrix( ll ) => DenseMatrix( ll.transpose ) }
  //def vector:Parser[Expr] = "[" ~> rep1sep( factor, ' ' ) <~ "]" ^^ { case l => Matrix( List( l ) }
  def row:Parser[Expr] = "[" ~> rep1sep( m_expression, " " ) <~ "]" ^^ { case l:List[Expr] => DenseMatrix( List( l ) ) }
  def matrixCore:Parser[Expr] = "[" ~> rep1sep( rep1sep( m_expression, ' ' ), ";" ) <~ "]" ^^ Mat
  def rowVector:Parser[Expr] = 
    m_expression ~ ":" ~ m_expression ~ ":" ~ m_expression ^^ { case a~_~b~_~c => RowVector( a, c, b ) } |
    m_expression ~ ":" ~ m_expression                      ^^ { case a~_~b     => RowVector( a, b, Number( 1 ) ) }


  def linspace:Parser[Expr] =
    "linspace" ~ "(" ~> floatingPointNumber ~ "," ~ floatingPointNumber ~ "," ~ wholeNumber <~ ")" ^^ { case a~_~b~_~c => LinSpace( a.toDouble, b.toDouble, c.toInt ) } |
    "linspace" ~ "(" ~> floatingPointNumber ~ "," ~ floatingPointNumber <~ ")" ^^ { case a~_~b => LinSpace( a.toDouble, b.toDouble) }
    def logspace:Parser[Expr] =
    "logspace" ~ "(" ~> floatingPointNumber ~ "," ~ floatingPointNumber ~ "," ~ wholeNumber <~ ")" ^^ { case a~_~b~_~c => LogSpace( a.toDouble, b.toDouble, c.toInt ) } |
    "logspace" ~ "(" ~> floatingPointNumber ~ "," ~ floatingPointNumber <~ ")" ^^ { case a~_~b => LogSpace( a.toDouble, b.toDouble ) }

  def matrix:Parser[Expr] = 
    eye | 
    ones | zeros | rand | linspace | logspace | transpose |
    matrixCore <~ ''' ^^ { case DenseMatrix( ll ) => DenseMatrix( ll.transpose ) } | 
    matrixCore 
  def transpose:Parser[Expr] = "transpose" ~> "(" ~> m_expression <~ ")" ^^ { case e => Transpose( e ) }
  
  val NumD = ( a:String ) => Number( a.toDouble )
  //val Der = ( a:Expr, b:Variable ) => Derivative( a, b ) //a.derive( b ) //Derivative( a, b ) //a.derive( b )
  
  val Mat = (ll:List[List[Expr]]) => { require( ll.forall( str => str.size == ll(0).size ), "Not all rows have the same size" ); DenseMatrix( ll ) }
}

// functions
trait functionParser extends JavaTokenParsers {
  val expression:Parser[Expr]
  def function:Parser[ Expr ] = 
    //"kron" ~> "(" ~> expression ~ ( "," ~> expression <~ ")" ) ^^ TensorProduct |
    "sin" ~> "(" ~> expression <~ ")" ^^ SinF1 |
    "cos" ~> "(" ~> expression <~ ")" ^^ CosF1 |
    "tan" ~> "(" ~> expression <~ ")" ^^ TanF1 |
    "asin" ~> "(" ~> expression <~ ")" ^^ AsinF1 |
    "acos" ~> "(" ~> expression <~ ")" ^^ AcosF1 |
    "atan" ~> "(" ~> expression <~ ")" ^^ AtanF1 |
    "log" ~> "(" ~> expression <~ ")" ^^ LogF1 |
    "exp" ~> "(" ~> expression <~ ")" ^^ ExpF1 |
    "sqrt" ~> "(" ~> expression <~ ")" ^^ { e:Expr => Sqrt( e ) } |
    "info" ~> "(" ~> expression <~ ")" ^^ Info |
    "eval" ~> "(" ~> expression <~ ")" ^^ Eval |
    "simplify" ~> "(" ~> expression <~ ")" ^^ Simplify |
    "expand" ~> "(" ~> expression <~ ")" ^^ Expand |
    "factor" ~> "(" ~> expression <~ ")" ^^ Factor
}

// logicalParser
trait logicParser extends JavaTokenParsers with ImplicitConversions {
  val expression:Parser[Expr]
  // logical operations
  def bool = "true" ^^ { _ => Bool( true ) } | "false" ^^ { _ => Bool( false ) } 
  def boolOps:Parser[Expr] = boolAnd | boolOr | boolXor | boolNot
  def boolNot:Parser[Expr] = "not" ~> "(" ~> expression <~ ")" ^^ { case e => BoolNot( e ) }
  def boolAnd:Parser[Expr] = ( "and" ~> "(" ~> expression ) ~ ( "," ~> expression <~ ")"  ) ^^ { case e~f => BoolAnd( e, f ) } 
  def boolOr:Parser[Expr] = ( "or" ~> "(" ~> expression ) ~ ( "," ~> expression <~ ")"  ) ^^ { case e~f => BoolOr( e, f ) } 
  def boolXor:Parser[Expr] = ( "xor" ~> "(" ~> expression ) ~ ( "," ~> expression <~ ")" ) ^^ { case e~f => BoolXor( e, f ) }
}

trait builtinParser extends JavaTokenParsers { //extends JavaTokenParsers with ImplicitConversions{ 
  //system commands, used by builtin
  def comment:Parser[Expr] = "//" ~ rep(not("\n") ~ ".".r) ^^^ { new NilExpr }
  //"//" ~ rep(not("\n") ~ ".".r) ^^^ Unit
  def clear:Parser[Expr] = "clear" ^^ { case _ => new Clear() }
  def exit:Parser[Expr] = "exit" ^^ { case _ => new Exit() }
  def ls:Parser[Expr] = "ls" ^^ { case _ => SystemCommand( "ls" ) }
  def cd:Parser[Expr] = "cd" ~> path ^^ { p => SystemCommand( "cd " + p ) }
  def pwd:Parser[Expr] = "pwd" ^^ { case _ => SystemCommand( "pwd ") }
  def load:Parser[Expr] = "load" ~ "(" ~> path <~ ")" ^^ { case filename => Load( filename ) } 
  def who:Parser[Expr] = "who" ^^ { case(_:String) => new Who() }
  def whos:Parser[Expr] = "whos" ^^ { case(_:String) => new Whos() }
  def path:Parser[String] = "[^)]+".r // ^^ { _.toString }
}
