import galileo.parser._
import galileo.constants._
import galileo.exprhandler._
import galileo.environment._
import galileo.expr._
import galileo.linalg._
import galileo.trigonometry.SinF1

import org.jline.reader.{LineReader,LineReaderBuilder}

object Shell {
  val parser = new Parser()
  val handler = new ExprHandler 
  val genv = new Environment( None ) // Global env
  genv.set( "pi", new ConstantPi )
  genv.set( "e", new ConstantE )
  genv.set( "j", new ConstantJ )
  genv.set( "i", new ConstantJ )
  val senv = new Environment( Some( genv ) ) // Session env
  /*
  senv.set( "A", DenseMatrix( List( 
    List( 0, 5, 5 ).map( e => Number( e ) ), 
    List( 6, 8, 8 ).map( e => Number( e ) ),
    List( 2, 9, 0 ).map( e => Number( e ) )
  ) ) )

  senv.set( "B", DenseMatrix( List( 
    List( 8, 2, 9 ).map( e => Number( e ) ), 
    List( 4, 9, 4 ).map( e => Number( e ) ),
    List( 6, 7, 9 ).map( e => Number( e ) )
  ) ) )

  senv.set( "D", DenseMatrix( List( 
    List( 11,  9, 24, 2 ).map( e => Number( e ) ),
    List(  1,  5,  2, 6 ).map( e => Number( e ) ),
    List(  3, 17, 18, 1 ).map( e => Number( e ) ),
    List(  2,  5,  7, 1 ).map( e => Number( e ) )
  ) ) )

  senv.set( "b", DenseMatrix( List( List( Number( 4 ) ), List(Number(5)), List( Number( 6 )))))
  senv.set( "A0", DenseMatrix( List( 
    List( 1, 0, 0).map( e => Number( e ) ), 
    List( 0, 0, 1).map( e => Number( e ) ),
    List( 0, 1, 0).map( e => Number( e ) )
  ) ) )
   senv.set( "A1", DenseMatrix( List( 
    List( 0, 0, 4).map( e => Number( e ) ),
    List( 0, 2, 0).map( e => Number( e ) ),
    List( 1, 0, 0).map( e => Number( e ) )
  ) ) )

  senv.set( "A2", DenseMatrix( List( 
    List( 1, 0, 0).map( e => Number( e ) ), 
    List( 0, 1, 0).map( e => Number( e ) ),
    List( 0, 0, 1).map( e => Number( e ) )
  ) ) )
  senv.set( "A3", DenseMatrix( List( 
    List( 1, 2, 3).map( e => Number( e ) ), 
    List( 4, 5, 6).map( e => Number( e ) ),
    List( 7, 8, 0).map( e => Number( e ) )
  ) ) )
  
  senv.set( "A4", DenseMatrix( List( 
    List( 1, -2, 3).map( e => Number( e ) ), 
    List( 2, -5, 12).map( e => Number( e ) ),
    List( 0, 2, -10).map( e => Number( e ) )
  ) ) )

  senv.set( "C", DenseMatrix( List( 
    List( 3, 3,  4 ).map( e => Number( e ) ), 
    List( 3, 5,  9 ).map( e => Number( e ) ),
    List( 5, 9, 17 ).map( e => Number( e ) )
  ) ) )
  senv.set( "c", DenseMatrix( 
    List( 1, 2, 4 ).map( elem => List( Number( elem ) ) )
  ) )
  senv.set( "d", DenseMatrix( 
    List( 3, 2, 4, 5 ).map( elem => List( Number( elem ) ) )
  ) )
  senv.set( "M", DenseMatrix( List( 
    List( 1, 2, 3 ).map( e => Number( e ) ), 
    List( 4, 5, 6 ).map( e => Number( e ) ),
    List( 7, 8, 0 ).map( e => Number( e ) )
  ) ) )
  */
  senv.set( "A", DenseMatrix( List( 
    List( "a", "b" ).map( e => Variable( e ) ), 
    List( "c", "d" ).map( e => Variable( e ) )
  ) ) )

  val MinusR2 = Product( Number( -1 ), Power( Variable( "r" ), Number( 2 ) ) )
  
  senv.set( "metric", DenseMatrix( List( 
    List( 1,  0, 0, 0 ).map( e => Number( e ) ), 
    List( 0, -1, 0, 0 ).map( e => Number( e ) ), 
    List( Number( 0 ), Number( 0 ), MinusR2, Number( 0 ) ), 
    List( Number( 0 ), Number( 0 ), Number( 0 ), Product( MinusR2, Power( SinF1( Variable( "theta" ) ), Number( 2 ) ) ) ) 
  ) ) )
  senv.set( "coords", DenseMatrix( List(
    List( "t", "r", "theta", "phi" ).map( e => Variable( e ) )
  ) ) )
  

  //senv.set( "b", DenseMatrix( List( List( Number( 4 ) ), List(Number(5)), List( Number( 6 )))))
  
  
  def main(args: Array[String]) = loop()
    import parser.{ Success, NoSuccess }

    // New jline style approach for ConsoleReader
    val cr = LineReaderBuilder.builder().build();
 
    def loop() {
      while ( true ) {
        val exprSrc = cr.readLine( "galileo> ")
        parser.parse(exprSrc) match {   
          case Success( expressions, _ ) => expressions.foreach( expression => println( handler.eval( senv, expression ) ) )
          case err: NoSuccess   => println(err)
        }
    }
  }
}

  
