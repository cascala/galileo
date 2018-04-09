package galileo.exprhandler

import galileo.complex._
import galileo.constants._
import galileo.environment._
import galileo.expr._
import galileo.linalg._
import galileo.logic._
import galileo.proof.Proof
import galileo.rand.Rand
import galileo.solve.Solve
import galileo.trigonometry._


// Used by the Shell to handle expressions (Abstract Syntax Tree)
// I need to add an explicit eval step (replacing variables etc)
class ExprHandler{
  def eval(env:Environment, expr:Expr):String = {
    expr match {
      case Who() => env.toString
      case Whos() => env.toString
      case Clear() => { env.clear; "" }
      case s:SystemCommand => s.toString()
      case Exit() => { System.exit( 0 ); "" } // caught in Shell, rethrown there
      // Do we need to pass e by reference?
      case e:Expr => { val ans = visit( env, e ); env.set( "ans", ans ); ans.toString }
    }
  }

  def visit( env:Environment, e:Expr):Expr = { 
    e match {
      case Eval( e ) => visit( env, e ).eval()
  	
      case MatInv( MatInv( a ) ) => a.visit( Some( env ) ) // should in theory check for singular matrix here
      case m:MatInv  => m.visit( Some( env ) )
      case n:MatNorm => n.visit( Some( env ) )

      case d:Derivative => d.visit( Some( env ) )
  		case Number( v ) => Number( v ) // can't simplify that...
      //case NumberI( v ) => NumberI( v ) // or that
  		case v:Variable => v.visit( Some( env ) )
		  case c:Complex => c.visit( Some( env ) )
  		case s:Sum => s.visit( Some( env ) )
  		case p:Product => p.visit( Some( env ) )
      case f:Fraction => f.visit( Some( env ) )
      case s:Solve => s.visit( Some( env ) )
      case p:Proof => p.visit( Some( env ) )
      case i:Info => i.visit( Some( env ) )
      case r:Rand => r.visit( Some( env ) )
      case l:Load => l.visit( Some( env ) )

  		case Assignment2( n0, n1, MatLU( e ) ) => visit( env, e ) match {
  			case m:Matrix => m.lup_lup match{
				  case (l,u,p) => {
            val pil = p.solve( l.toDenseMatrix ) // inv(P)*L // would be more efficient to work with rows of L?!?!
            env.set( n0, pil )
            env.set( n1, u )
            ExprArray(
              Assignment1( n0, pil ),
              Assignment1( n1, u )
            )
          } 
        }
  			case _ => ErrorExpr( "function 'lu(matrix)' can only be applied to a matrix" )
  		}
      case a:Assignment2 => ErrorExpr( "right hand side can not produce two variables" )
  		case Assignment3( n0, n1, n2, MatLU( e ) ) => visit( env, e ) match {
  			case m:Matrix => m.lup_lup match {
          case (l,u,p) => {
            env.set( n0, l )
            env.set( n1, u )
            env.set( n2, p ) 
            ExprArray(
              Assignment1( n0, l ),
              Assignment1( n1, u ),
              Assignment1( n2, p )
            )
          }
          case _ => ErrorExpr( "function 'lu(matrix)' can only be applied to a matrix" )
        }
  			case _ => ErrorExpr( "function 'lu(matrix)' can only be applied to a matrix" )
  		}
      case a:Assignment3 => ErrorExpr( "right hand side can not produce three variables" )

  		
  		case Assignment1( name, value ) => { 
				val vv = visit( env, value ) 
				env.set( name, vv )
				vv 
			}
  		
  		case p:Power => p.visit( Some( env ) )

  		case m:Matrix => m.visit( Some( env ) ) //Matrix( ll.map( row => row.map( elem => visit( elem ) ) ) )
    
      case u:OnesMatrixU => u.visit( Some( env ) )
  	
  		case b:Bool => b
  	
    	case BoolAnd( e,f ) => ( visit( env, e ), visit( env, f ) ) match {
  			case ( l:Bool, r:Bool) => l & r
  			case ( l:Bool, r:Number ) => l & r
  			case ( l:Number, r:Bool ) => l & r
  			case ( l:Number, r:Number ) => l & r
   			case ( l:Expr, r:Expr ) => BoolAnd( l, r ) //throw new Exception( "Not a boolean")
  		}

  		case BoolOr( e,f ) => (visit( env, e ), visit( env, f ) ) match {
  			case ( l:Bool, r:Bool) => l | r
  			case ( l:Bool, r:Number ) => l | r
  			case ( l:Number, r:Bool ) => l | r
  			case ( l:Number, r:Number ) => l | r
   			case ( l:Expr, r:Expr ) => BoolOr( l, r ) //throw new Exception( "Not a boolean")
  		}

  		case BoolAndSc( e,f ) => (visit( env, e ), visit( env, f ) ) match {
  			case ( l:Bool, r:Bool) => l && r
  			case ( l:Bool, r:Number ) => l && r
  			case ( l:Number, r:Bool ) => l && r
  			case ( l:Number, r:Number ) => l && r
   			case ( l:Expr, r:Expr ) => { println( "l.info" + l.info() + ", r.info():" + r.info() ); BoolAndSc( l, r ) } //throw new Exception( "Not a boolean")
  		}

  		case BoolOrSc( e,f ) => (visit( env, e ), visit( env, f ) ) match {
  			case ( l:Bool, r:Bool) => l || r
  			case ( l:Bool, r:Number ) => l || r
  			case ( l:Number, r:Bool ) => l || r
  			case ( l:Number, r:Number ) => l || r
   			case ( l:Expr, r:Expr ) => BoolOrSc( l, r ) //throw new Exception( "Not a boolean")
  		}

  		case BoolXor( e,f ) => ( visit( env, e ), visit( env, f ) ) match {
  			case ( l:Bool, r:Bool) => l ^ r
  			case ( l:Bool, r:Number ) => l ^ r
  			case ( l:Number, r:Bool ) => l ^ r
  			case ( l:Number, r:Number ) => l ^ r
   			case ( l:Expr, r:Expr ) => BoolXor( l, r ) //throw new Exception( "Not a boolean")
  		}

  		case BoolNot( e ) => visit( env, e ) match {
  			case l:Bool => !l
  			case l:Number => !l
   			case l:Expr => BoolNot( l ) //, r ) //throw new Exception( "Not a boolean")
  		}

  		// special handling for things like Pi and e
  		//case CosF1( AcosF1( e ) ) => visit( e )
  		//case SinF1( AsinF1( e ) ) => visit( e )
  		//case TanF1( AtanF1( e ) ) => visit( e )
		  case AcosF1( CosF1( e ) ) => visit( env, e )
  		case AsinF1( SinF1( e ) ) => visit( env, e )
  		case AtanF1( TanF1( e ) ) => visit( env, e )

		  /*case CosF1( Number( v ) ) => Number( math.cos( v ) )
  		case SinF1( Number( v ) ) => Number( math.sin( v ) )
  		case TanF1( Number( v ) ) => Number( math.tan( v ) )
  		*/

  		case c:CosF1 => c.visit( Some( env ) )
  		case s:SinF1 => s.visit( Some( env ) )
  		case t:TanF1 => t.visit( Some( env ) )
  		
  		case ac:AcosF1 => ac.visit( Some( env ) )
  		case as:AsinF1 => as.visit( Some( env ) )
  		case at:AtanF1 => at.visit( Some( env ) )

  		case j:ConstantJ => visit( env, j.value )
  		case c:Constant => c // don't expand down for pi and e
      case LogF1( c:ConstantE ) => Number( 1 )
      case ExpF1( LogF1( e ) ) => visit( env, e )
      case LogF1( ExpF1( e ) ) => visit( env, e )
      case l:LogF1 => l.visit( Some( env ) )
      case l:ExpF1 => l.visit( Some( env ) )
  		//case c:Constant => visit( c.value ) // don't expand down for pi and e

      case whatever => whatever.visit( Some( env ) )
    } // match
  } // visit
} // class ExprHandler
