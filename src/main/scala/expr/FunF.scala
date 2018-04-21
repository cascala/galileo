package galileo.expr

import galileo.environment.Environment

// Functions of one variable (or expression)
// used in SinF1, LogF1...
trait FunF1 extends Expr {
	val e:Expr
	override def leadingVariable:Option[String] = e.leadingVariable
	def variables:List[Variable] = e.variables
}

// Functions of two variables (or expressions)
// Use in Power, Fraction...
trait FunF2 extends Expr {
	val a:Expr
	val b:Expr
	override def leadingVariable:Option[String] = (a.leadingVariable,b.leadingVariable) match {
		case (Some(r),Some(s)) => Some( List( r, s ).min )
		case (Some(r),_) => Some(r)
		case (_,Some(s)) => Some(s)  
		case _ => None
	}
  def info(env:Option[Environment]=None) = this.getClass.getSimpleName + "(" + a + "," + b + ")"
  def variables:List[Variable] = a.variables ++ b.variables
}

// Functions of any number of variables (or expressions)
// Used by Sum and Product
trait FunMany extends Expr {
 	val elements:List[Expr]
  	
    // Possible variable name 
  	override def leadingVariable:Option[String] = {
    	//val lvs = this.elements.map( element => element.leadingVariable ).filter( pv => pv != None ).map( {case Some( s ) => s })
      val lvs = this.elements.map( element => element.leadingVariable ).map( { 
        case Some( s ) => s
        case _ => ""
      } )
    	lvs.size > 0 match {
      		case true => Some( lvs.min )
      		case false => None
    	}
  	}

	// A generic function that applies pair-wise simplification
	// neutralElement is 0 for Sum, 1 for Product
  	def scan(neutralElement:Expr,terms:List[Expr],scanner:(Expr,Expr)=>Option[Expr]):List[Expr] = {
        // simply loop over all factors and combine if/when possible
        var newTerms:List[Expr] = Nil
        var candidate:Expr = neutralElement
        for( term <- terms ) {
        	scanner(candidate,term ) match {
        		case Some(r) => candidate = r
        		case _ => {
        			//println( "FunMany.scan: scanner(" + candidate + "," + term + "):" + scanner( candidate, term ) )
        			newTerms = newTerms :+ candidate
            	candidate = term  
        		}
        	}
        }
        // add the last one...
        newTerms :+ candidate
    }
  
  // turn an List of Expr into an Expr:
  // * terms become a Sum
  // * factors become a Product
  def expressify(l:List[Expr]):Expr
  //def neutralElement:Number

  def variables:List[Variable] = elements.flatMap( element => element.variables )
}
