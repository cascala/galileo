package galileo.expr

import galileo.complex._
import galileo.constants.ConstantE
import galileo.environment._
import galileo.linalg._
import galileo.logic._
//import galileo.proof.Conversion
import galileo.trigonometry._

trait Expr{
	def derive(v:Variable):Expr = this match {
		case Number(_) => Number( 0 )
		case Variable( name ) => if( name == v.name ) Number( 1 ) else Number( 0 )
		case Sum( e1, e2 ) => Sum( e1 derive v, e2 derive v )
		case Product( e1, e2 ) => Sum( Product( e1, e2 derive v), Product( e1 derive v, e2 ) )
		case Power( e1, e2 ) => Product( e2, Power( e1, Sum( e2, Number( -1 ) ) ) )
		case ExpF1( e ) => Product( ExpF1( e ), e derive v )
		case LogF1( e ) => Product( Power( e, Number( -1 ) ), e derive v )
		case CosF1( e ) => Product( Product( Number( -1 ), SinF1( e  ) ), e derive v )
		case SinF1( e ) => Product( CosF1( e  ), e derive v )

	} // def derive

	def visit(env:Option[Environment]=None):Expr = this
	def eval():Expr = this
	
	// Manipulation
	def simplify:Expr = this
	def expand:Expr = this
	def factor:Expr = this

	//def factors
	def toString():String
	def factorToString():String = toString() // if printed as a factor, e.g as a in 5 * a or as (-1+b) in a * ( -1 + b )
	def denominatorToString():String = factorToString() // if printed as a denominator, e.g. a*b in 1/(a*b)
	def toStringWithSign():String = toString()

	def >(d:Double):Boolean = false
	def <(d:Double):Boolean = false
	val doubleValue:Double = Double.MinValue

	def unary_-():Expr = Product( Number( -1 ), this ).visit()
	def +(that:Expr) = Sum( this, that ).visit()
	def -(that:Expr) = Sum( this, Product( Number( -1 ), that ) )
	//def *(that:Expr) = Product( this, that ).visit()
	//def /(that:Expr) = Fraction( this, that ).visit()
	def info(env:Option[Environment]=None):String // abstract, needs to be defined = "Expr(" + toString() + ")"
	//def conversions(depth:Int):List[Conversion]

	// Possibly extract factor e from an expression
	// (a).extractFactor(a) -> Number( 1 )
	def extractFactor(e:Expr):Option[Expr] = e match {
		case Number( 1 ) => Some( this )
		case s if ( s == this ) => Some( Number( 1 ) )
		case _ => None
	}

	def flatFactors:List[Expr] = List( this )
	def flatTerms:List[Expr] = List( this )
	def possibleFactors:List[Expr] = List( this )
	def leadingVariable:Option[String] = None
	def <(that:Expr):Boolean = (this.leadingVariable,that.leadingVariable) match { 
		case (Some(a),Some(b)) => a < b
		case (_,Some(b)) => false // Plain numbers go after expressions with variables
		case (Some(a),_) => true
		case (_,_) => false // whatever...
	}
}

// Needed for things like comments, etc, which don't express anything
class NilExpr extends Expr {
	def info(env:Option[Environment]=None) = "NilExpr"
}

case class ErrorExpr( message:String = "Unknown error" ) extends Expr {
	override def toString  = message
	def info(env:Option[Environment]=None) = "ErrorExpr(" + message + ")"
}

case class StringExpr( message:String ) extends Expr {
	override def toString  = message
	def info(env:Option[Environment]=None) = "StringExpr(" + message + ")"
}

case class ExprArray(exprs:Expr*) extends Expr {
	override def toString() = exprs.mkString( "\n" ) //( e:Expr => e.toString() )
	def info(env:Option[Environment]=None) = "ExprArray(" + exprs + ")"
	override def visit(env:Option[Environment]=None):Expr = {
		ExprArray( exprs.map( expression => expression.visit( env ) ):_* )
	}
}

// ls, pwd
case class SystemCommand( command:String ) extends Expr {
	def info(env:Option[Environment]=None) ="SystemCommand(" + command + ")"
	import sys.process._
	override def toString() = {
		import scala.language.postfixOps
		command !! // does not work for cd
	}
}

// who
case class Who() extends Expr {
	def info(env:Option[Environment]=None) = "Who()"
}

// whos
case class Whos() extends Expr {
	def info(env:Option[Environment]=None) = "Whos()"
}

// clear
case class Clear() extends Expr {
	def info(env:Option[Environment]=None) = "Clear()"
}

// exit
case class Exit() extends Expr {
	def info(env:Option[Environment]=None) = "Exit()"
}

// a = ...
case class Assignment(name:String, expr:Expr) extends Expr{
	override def toString() = name + "\t=\n" + expr.toString()
	def info(env:Option[Environment]=None) = "Assignment(" + name + "," + expr + ")"
	override def visit(env:Option[Environment]) = {
		val vv = expr.visit( env )
		env match {
			case Some( e ) => { e.set( name, vv ); this }
			case None => ErrorExpr( "Internal error" )
		}
	}
}

// [ A,B] = ...
case class Assignment2(name0:String, name1:String, expr:Expr ) extends Expr{
	override def toString() = "[" + name0 + "," + name1 + "]\t=\n" + expr.toString() 
	def info(env:Option[Environment]=None) = "Assignment2(" + name0 + "," + name1 + "," + expr + ")"
	override def visit(env:Option[Environment]) = ErrorExpr( "Todo Assignment2.visit" ) //{ val vv = expr.visit( env ); env.set( name, vv ); vv }
}

// [A,B, C] = ...
case class Assignment3(name0:String, name1:String, name2: String, expr:Expr ) extends Expr{
	override def toString() = "[" + name0 + "," + name1 + "," + name2 + "]\t=\n" + expr.toString() 
	def info(env:Option[Environment]=None) = "Assignment3(" + name0 + "," + name1 + "," + name2 + "," + expr + ")"
	override def visit(env:Option[Environment]) = ErrorExpr( "Todo Assignment3.visit" ) //{ val vv = expr.visit( env ); env.set( name, vv ); vv }
}




case class Variable(name : String = "x") extends Expr {
	override def toString() = name
	def info(env:Option[Environment]=None):String = "Variable(" + name + ",value:" + this.visit(env) + ")"
	override def toStringWithSign() = "+" + name

	
	override def visit(env:Option[Environment]=None):Expr = env match {
		case Some( e ) => e.get( this.name ) match {
			//case Some( c:Constant ) => c // this doesn't work well... What is var is a = 2 * pi was set?
			case Some( v ) => v.visit( env )
//				v.visit( Some( e.getWithout( this.name ) ) ) // avoid infinite loops
//				v.visit( env )
//			}
			case None => this
		}
		case None => this
	}
	override def possibleFactors:List[Expr] = List( this )
	override def leadingVariable:Option[String] = Some( name )
}

case class FuncCall(func:Expr, params:List[Expr]) extends Expr {
	def info(env:Option[Environment]=None):String = "FuncCall(" + func + "," + params + ")"
}

case class Eval( e:Expr ) extends Expr {
	override def visit( env:Option[Environment]=None):Expr = e.eval()
	def info(env:Option[Environment]=None):String = "Eval(" + e + ")"
}

