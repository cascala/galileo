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
import galileo.tensor.TensorProduct
import galileo.rand.Rand

class Parser extends JavaTokenParsers with ImplicitConversions { 
	// lexer can be used to reserve keywords
  def program = rep1sep(statement, ";") <~ opt(";")
  //def program = rowVector ^^ { a => List( Number( 1 ), Number( 3 ) ) }
  def statement:Parser[Expr] =  builtin | prove | assignment | expr
  def prove:Parser[Expr] = "prove" ~ "(" ~> expr ~ ( "=" | ">" | "<" | "!=" ) ~ expr <~ ")" ^^ { case l~o~r => Proof( l, o, r ) }
  //def sqrt:Parser[Expr] = "sqrt" ~ "(" ~> expr <~ ")" ^^{ e:Expr => Sqrt(e) }
  def rand:Parser[Expr] = 
    "rand" ~ "(" ~> expression ~ "," ~ expression <~ ")" ^^ { case nr~_~nc => Rand( nr,nc ) } |
    "rand" ~ "(" ~> expression <~ ")" ^^ { case e => Rand( e ) }
  
  def expr = expression // | deriv | mats | eval// | expression//boolOps | terms
  def expression:Parser[Expr] = /*unaryMinus |*/ chainl1( term, "+" ^^^ Sum2 | "-" ^^^ Sub | "||" ^^^ Or2 ) //| rowVector//  basic math terms (no deriv etc)
  def term:Parser[Expr] = chainl1( power, "*" ^^^ Product2 | "/" ^^^ Div | "&&" ^^^ And2 ) //| rowVector
  def power:Parser[Expr] = chainl1( factor, "^" ^^^ Power ) 
  def factor:Parser[Expr] = mats | function | deriv | boolOps |
    // wholeNumber ^^ NumI 
    rowVector |
    floatingPointNumber ^^ NumD | 
    bool | 
    variableE | //bool | //rowVector |
    "(" ~> expression <~ ")" | unaryMinus | "" ~> failure( "factor expected")

  def unaryMinus:Parser[Expr] = "-" ~> term ^^ { e:Expr => Product( Number( -1 ), e ) }

  //def tensor = "tensor" ~> "(" ~> expr ~ "," ~ expr ~ "," ~ expr <~ ")" ^^ { case l~_~u~_~d => TensorU( l, u, d ) } 

  //def tensor = christof // "hello" ^^^{ Number( 1 ) } //christof
 // def christof:Parser[Expr] = //"christof" ~> "(" ~> expression ~ ( "," ~> vector <~ ")" ) ^^ { case metric~coords => Christoffel( metric, coords, 1 ) } |
  //  "christof" ~> "(" ~> expression ~ ( "," ~> expression <~ ")" ) ^^ { case metric~coords => Christoffel( metric, coords, 1 ) }
  //def vector:Parser[List[Expr]] = "[" ~> rep1sep( expression, " " ) <~ "]" ^^ { case es => es.to[List] }

  // content valid inside a matrix  
  def matrixContent:Parser[Expr] = expression //unaryMinusMatrixTerm | matrixTerm | // | 
    //chainl1( matrixTerm, "+" ^^^ Sum2 | "-" ^^^ Sub ) //| rowVector//  basic math terms (no deriv etc)
    //rep1sep( matrixTerm, "+" ) ^^ { case a => Sum( a ) } 
  def matrixTerm:Parser[Expr] = chainl1( matrixPower, "*" ^^^ Product2 | "/" ^^^ Div ) //| rowVector
  def matrixPower:Parser[Expr] = chainl1( matrixFactor, "^" ^^^ Power ) 
  def matrixFactor:Parser[Expr] = //mats | eval | function | deriv | boolOps | 
    // wholeNumber ^^ NumI 
    //rowVector |
    //unaryMinusMatrixContent |
    floatingPointNumber ^^ NumD | 
    //boolExpr | 
    variableE | //rowVector |
    "(" ~> matrixContent <~ ")" | "" ~> failure( "factor expected")  
  def unaryMinusMatrixTerm:Parser[Expr] = "-" ~> matrixTerm ^^ { e:Expr => Product( Number( -1 ), e ) }
  //def matrixContent:Parser[Expr] = expression //| floatingPointNumber ^^ NumD | deriv | eval | variableE
  
  // system commands
  def builtin:Parser[Expr] = whos | who | clear | ls | cd | pwd | load | exit
  def clear:Parser[Expr] = "clear" ^^ { case _ => new Clear() }
  def exit:Parser[Expr] = "exit" ^^ { case _ => new Exit() }
  def ls:Parser[Expr] = "ls" ^^ { case _ => SystemCommand( "ls" ) }
  def cd:Parser[Expr] = "cd" ~> path ^^ { p => SystemCommand( "cd " + p ) }
  def pwd:Parser[Expr] = "pwd" ^^ { case _ => SystemCommand( "pwd ") }
  def load:Parser[Expr] = "load" ~ "(" ~> path <~ ")" ^^ { case filename => Load( filename ) } 
  def who:Parser[Expr] = "who" ^^ { case(_:String) => new Who() }
  def whos:Parser[Expr] = "whos" ^^ { case(_:String) => new Whos() }
  def path:Parser[String] = "[^)]+".r // ^^ { _.toString }

  // Assignment and array assignment (AssignmentN)
  def assignment:Parser[Expr] = 
    ( "[" ~> ident <~ "," ) ~ (ident <~ "," ) ~ ( ident <~ "]" ) ~ ( "=" ~> expr ) ^^ { case a~b~c~e => Assignment3( a, b, c, e ) } |
    ( "[" ~> ident <~ "," ) ~ ( ident <~ "]" ) ~ ( "=" ~> expr ) ^^ { case a~b~e => Assignment2( a, b, e ) } |
    ident ~ ( "=" ~> expr ) ^^ { case name~e => Assignment( name, e ) }

  // built-in math operations, like deriv, int, factor, eval, simpl...
  
  def deriv:Parser[Expr] = ( "deriv" ~> "(" ~> expression ) ~ ( "," ~> variableV <~ ")" ) ^^ { case y~x => Derivative( y, x ) }
  
  //def tensor:Parser[Expr] = "tensor" ~> "(" ~> mats <~ ")" ^^ { case m:Matrix => Tensor( m ) }
  def mats:Parser[Expr] = matsolve | matlu | matinv | matnorm | /* matkron | */matrix | matrixT
  def matsolve:Parser[Expr] = ( matrix | variableE ) ~ ( '\\' ~> ( matrix | variableE ) ) ^^ Solve
  def matlu:Parser[Expr] = "lu" ~> "(" ~> ( matrix | variableE )  <~ ")" ^^ MatLU
  def matinv:Parser[Expr] = "inv" ~> "(" ~> ( expression )  <~ ")" ^^ MatInv
  def matnorm:Parser[Expr] = 
    ( "norm" ~> "(" ~> expression <~ "," ) ~ ( posNumber <~ ")" ) ^^ { case m~p => MatNorm( m, p ) } |
    "norm" ~> "(" ~> expression <~ ")" ^^ { case m => MatNorm( m ) }
  //def matkron:Parser[Expr] = "kron" ~> "(" ~> expression ~ ( "," ~> expression <~ ")" )^^ { case a~b => MatKron( a, b ) }// Tensor product
  def posNumber:Parser[Integer] = wholeNumber ^^ { s:String => { val r = s.toInt; require( r >= 0 ); r } } 
  def ones:Parser[Expr] = 
    ( "ones" ~> "(" ~> expression ) ~ ( "," ~> expression <~ ")" ) ^^ { case n~m => OnesMatrixU( n, m ) } |
    "ones" ~> "(" ~> expression <~ ")" ^^ { case e => OnesMatrixU( e, e ) } |
    "ones" ^^ { _ => Number( 1 ) }
  def zeros:Parser[Expr] = 
    ( "zeros" ~> "(" ~> expression ) ~ ( "," ~> expression <~ ")" ) ^^ { case n~m => OnesMatrixU( n, m, Number( 0 ) ) } |
    "zeros" ~> "(" ~> expression <~ ")" ^^ { case e => OnesMatrixU( e, e, Number( 0 ) ) } |
    "zeros" ^^ { _ => Number( 0 ) }

  def eye:Parser[Expr] = 
    ( "eye" ~> "(" ~> expression ) ~ ( "," ~> expression <~ ")" ) ^^ { case n~m => EyeMatrixU( n, m ) } |
    "eye" ~> "(" ~> expression <~ ")" ^^ { case e => EyeMatrixU( e, e ) } |
    "eye" ^^ { _ => Number( 1 ) }

  // TODO: true and false are not valid variable names!!!
  def variableV:Parser[Variable] = ident ^^ Var
  def variableE:Parser[Expr] = ident ^^ Var
  //def vectorT:Parser[Expr] = vector <~ ''' ^^ { case Vector( l ) => Matrix( l.map( e=> List(e) ) ) }//VecT //| matrix <~ ''' MatT
  def matrixT:Parser[Expr] = matrix <~ ''' ^^ { case DenseMatrix( ll ) => DenseMatrix( ll.transpose ) }
  //def vector:Parser[Expr] = "[" ~> rep1sep( factor, ' ' ) <~ "]" ^^ { case l => Matrix( List( l ) }
  def matrixCore:Parser[Expr] = "[" ~> rep1sep( rep1sep( matrixContent, ' ' ), ";" ) <~ "]" ^^ Mat
  def rowVector:Parser[Expr] = 
    floatingPointNumber ~ ":" ~ floatingPointNumber ~ ":" ~ floatingPointNumber ^^ { case a~_~b~_~c => RowVector( Number( a.toDouble ), Number( c.toDouble ), Number( b.toDouble ) ) } |
    floatingPointNumber ~ ":" ~ floatingPointNumber                             ^^ { case a~_~b     => RowVector( Number( a.toDouble ), Number( b.toDouble ), Number( 1          ) ) } 
  def linspace:Parser[Expr] =
    "linspace" ~ "(" ~> floatingPointNumber ~ "," ~ floatingPointNumber ~ "," ~ wholeNumber <~ ")" ^^ { case a~_~b~_~c => LinSpace( a.toDouble, b.toDouble, c.toInt ) } |
    "linspace" ~ "(" ~> floatingPointNumber ~ "," ~ floatingPointNumber <~ ")" ^^ { case a~_~b => LinSpace( a.toDouble, b.toDouble) }
    def logspace:Parser[Expr] =
    "logspace" ~ "(" ~> floatingPointNumber ~ "," ~ floatingPointNumber ~ "," ~ wholeNumber <~ ")" ^^ { case a~_~b~_~c => LogSpace( a.toDouble, b.toDouble, c.toInt ) } |
    "logspace" ~ "(" ~> floatingPointNumber ~ "," ~ floatingPointNumber <~ ")" ^^ { case a~_~b => LogSpace( a.toDouble, b.toDouble ) }
  //   
  def matrix:Parser[Expr] = 
    eye | 
    ones | zeros | rand | linspace | logspace | transpose |
    //rowVector |
    matrixCore <~ ''' ^^ { case DenseMatrix( ll ) => DenseMatrix( ll.transpose ) } | 
    matrixCore //|
    // rowVector
  //def matrix:Parser[Expr] = "[" ~> rep1sep( vector, "," ) <~ "]" ^^ Matrix

  def transpose:Parser[Expr] = "transpose" ~> "(" ~> expression <~ ")" ^^ { case e => Transpose( e ) }
 

  // logical operations
  def bool = "true" ^^ { _ => Bool( true ) } | "false" ^^ { _ => Bool( false ) } 
  //def boolExpr = boolOps //| bool | "(" ~> bool <~ ")" | "(" ~> boolOps <~ ")" //boolOps | bool //factor // boolOps | expression | deriv // not applied to matrices?
  def boolOps:Parser[Expr] = boolAnd | boolOr | boolXor | boolNot
  def boolNot:Parser[Expr] = "not" ~> "(" ~> expr <~ ")" ^^ { case e => BoolNot( e ) }
  //def boolAndSc:Parser[Expr] = boolExpr ~ ( "&&" ~> boolExpr ) ^^ { case e~f => BoolAndSc( e, f ) } // short circuit definitions 
  //def boolOrSc:Parser[Expr] = boolExpr ~ ( "||" ~> boolExpr ) ^^ { case e~f => BoolOrSc( e, f ) } 
  def boolAnd:Parser[Expr] = ( "and" ~> "(" ~> expr ) ~ ( "," ~> expr <~ ")"  ) ^^ { case e~f => BoolAnd( e, f ) } 
  def boolOr:Parser[Expr] = ( "or" ~> "(" ~> expr ) ~ ( "," ~> expr <~ ")"  ) ^^ { case e~f => BoolOr( e, f ) } 
  def boolXor:Parser[Expr] = ( "xor" ~> "(" ~> expr ) ~ ( "," ~> expr <~ ")" ) ^^ { case e~f => BoolXor( e, f ) }
  
  val NumD = ( a:String ) => Number( a.toDouble )
  //val NumI = ( a:String ) => NumberI( a.toInt )

  
  // functions
  def function:Parser[ Expr ] = 
    "kron" ~> "(" ~> expression ~ ( "," ~> expression <~ ")" ) ^^ TensorProduct |
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

   //def eval:Parser[Expr] = ( "eval" ~> "(" ~> expression ) <~ ")" ^^ { case e => Eval( e ) }

  // Handlers, convert to right type
  val Der = ( a:Expr, b:Variable ) => Derivative( a, b ) //a.derive( b ) //Derivative( a, b ) //a.derive( b )
  val RowVector = ( begin:Expr, end:Expr, incr:Expr ) => (begin,end,incr) match {
    case (Number(b),Number(e),Number(a)) if ( a%1 == 0 ) => DenseMatrix( List( ( b to e by a ).map( n => Number( n) ).to[List] ))
    //case (Number(b),Number(e),Fraction(Number(a),Number(b)) => //case _ => 
  }

  //val MatrixSolve  = ( A:Matrix, b:Matrix ) => { require( A.numRows == A.numCols && A.numRows == b.numRows ); A.Solve( b ) }
  //val Sol = ( A:Matrix, b:Matrix ) => { require( A.numCols == b.numCols); MatrixSolve( A, b) }
  //val VecT = (l:List[Expr]) => Matrix( l.map( e => List( e ) ) )
  val Mat = (ll:List[List[Expr]]) => { require( ll.forall( str => str.size == ll(0).size ), "Not all rows have the same size" ); DenseMatrix( ll ) }
	//val Add = ( a:Expr, b:Expr ) => Series( a::b::Nil ) //Sum( a, b ) 
	val Sub = ( a:Expr, b:Expr ) => Sum( a, Product( Number( -1 ), b ) )
	//val Mul = ( a:Expr, b:Expr ) => Product( a, b )
	val Div = ( a:Expr, b:Expr ) => Fraction( a, b ) //Product( a, Power( b, Number( -1 ) ) )
  val Product2 = (a:Expr, b:Expr ) => Product( a, b )
  val Sum2 = (a:Expr, b:Expr ) => Sum( a, b )
  val Or2 = (a:Expr,b:Expr) => BoolOrSc( a, b )
  val And2 = (a:Expr,b:Expr) => BoolAndSc( a, b )
	//val Pow = ( a:Expr, b:Expr ) => Power( a, b )
	
	val Var = ( a:String ) => Variable( a ) //Variable( a.toString ) // as side effect, add to env?

	// VERY IMPORTANT
	// NEEDS parseAll rather than parse
	def parse(str:String):ParseResult[List[Expr]] = parseAll(program, str)
}
