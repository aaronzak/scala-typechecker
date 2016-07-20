import scala.io._
import cs162.assign3.syntax._
import Aliases._
import scala.io.Source.fromFile

//—————————————————————————————————————————————————————————————————————————
// Main entry point

object Checker {
  type TypeEnv = scala.collection.immutable.HashMap[Var, Type]
  object Illtyped extends Exception

  var typeDefs = Set[TypeDef]()

  def main( args:Array[String] ) {
    val filename = args(0)
    val input = fromFile(filename).mkString
    Parsers.program.run(input, filename) match {
      case Left(e) => println(e)
      case Right(program) =>
        val prettied = Pretty.prettySyntax(program)
        typeDefs = program.typedefs

        try {
          getType( program.e, new TypeEnv())
          println(Pretty.prettySyntax(program))
          println("This program is well-typed:\n")
        } catch { case Illtyped => println("This program is ill-typed") }
    }
  }

  // Gets all the constructors associated with a given type name.
  // For example, consider the following typedefs:
  //
  // type Either = Left num | Right bool
  // type Maybe = Some num | None
  //
  // With respect to the above typedefs, `constructors` will return
  // the following underneath the given arguments:
  //
  // constructors(Label("Either")) = Map(Label("Left") -> NumT, Label("Right") -> BoolT)
  // constructors(Label("Maybe")) = Map(Label("Some") -> NumT, Label("None") -> UnitT)
  // constructors(Label("Fake")) throws Illtyped
  //
  def constructors(name: Label): Map[Label, Type] =
    typeDefs.find(_.name == name).map(_.constructors).getOrElse(throw Illtyped)

  // Gets the type of the constructor.
  // For example, considering the typedefs given in the `constructors` comment above,
  // `typename` will return the following with the given arguments:
  //
  // typename(Label("Left")) = Label("Either")
  // typename(Label("Right")) = Label("Either")
  // typename(Label("Some")) = Label("Maybe")
  // typename(Label("None")) = Label("Maybe")
  //
  def typename(constructor: Label): Label =
    typeDefs.find(_.constructors.contains(constructor)).getOrElse(throw Illtyped).name

  def getType( e:Exp, env:TypeEnv ): Type =
    e match {
      // variables
      case x:Var => env get x match {
        case Some(t)=> t
        case _ => throw Illtyped

      } // FILL ME IN

      // numeric literals
      case _:Num => NumT // FILL ME IN

      // boolean literals
      case _:Bool => BoolT // FILL ME IN

      // `nil` - the literal for unit
      case _: NilExp => UnitT // FILL ME IN

      // builtin arithmetic operators
      case Plus | Minus | Times | Divide => {
      //  val args:Seq[Type] = Seq(NumT, NumT)
        FunT(Seq(NumT, NumT), NumT) 
        }// FILL ME IN

      // builtin relational operators
      case LT | EQ => {
        //val args:Seq[Type] = Seq(NumT, NumT)
        
        FunT(Seq(NumT, NumT), BoolT) 
        } 

      // FILL ME IN

      // builtin logical operators
      case And | Or => {
      //  val args:Seq[Type] = Seq(BoolT, BoolT)
        
        FunT(Seq(BoolT, BoolT), BoolT) 
        } // FILL ME IN

      // builtin logical operators
      case Not => {
      //  val args:Seq[Type] = Seq(BoolT)
        
        FunT(Seq(BoolT), BoolT) 
        } // FILL ME IN

      // function creation
      case Fun(params, body) => {
       // println("fun")
      FunT(params.map(_._2), getType(body,env ++ params)) 
      }// FILL ME IN

      // function call
      case Call(fun, args) => {
       // println("startCall")
        println(args.map(getType(_,env)))
        getType(fun,env) match{
          case FunT(p,r) => if(p==args.map(getType(_,env))) r else {
             println("call")
            throw Illtyped}
          case _ => throw Illtyped
        }
      } // FILL ME IN

      // conditionals 
      case If(e1, e2, e3) => {
       // println("startIF")

      if(getType(e1,env) == BoolT && getType(e2,env) == getType(e3,env)) getType(e2,env) else {
      //  println("if")
        throw Illtyped}

    }

      // FILL ME IN

      // let binding
      case Let(x, e1, e2) => {
        //println("let")
        getType(e2,env+(x->getType(e1,env)))

      } // FILL ME IN

      // recursive binding
      case Rec(x, t1, e1, e2) => {
    //  println("rec")
       
         if(getType(e1,env+(x->t1))==t1) getType(e2,env+(x->t1)) else {
        //  println("rec_check")
          throw Illtyped}

      } // FILL ME IN

      // record literals
      case Record(fields) => {
      //  println("record")
        
        RcdT(fields.map(pair => pair._1 -> getType(pair._2,env)))


      } // FILL ME IN

      // record access
      case Access(e, field) => {
       // println("access")
        getType(e,env) match{
                case RcdT(myFields) =>  myFields.get(field) match {
                  case Some(v) => v
                  case _ => throw Illtyped
                }
                
                case _ => throw Illtyped
              }

              } // FILL ME IN

      // constructor use
      case Construct(constructor, e) ⇒ {
      //  println("startConstruct")



         typeDefs.find(td => td.constructors.contains(constructor)) match {
          case Some(td) => if(getType(e,env) == td.constructors(constructor)) TypT(td.name) else {
           // println("construct")
            throw Illtyped
          }
          case _ => {
           // println("construct")
            throw Illtyped}

         }



        
      } // FILL ME IN

      // pattern matching (case ... of ...)
      case Match(e, cases) => {
      //  println("startMatch")
       val typeOfE = getType(e,env) 
       typeOfE match {
        case TypT(n) => { //TypT(name)
        

          if(constructors(n).map{ case (a,b) => a }.toSet != cases.map{ case (a,b,c) => a }.toSet ) throw Illtyped

          if( constructors(n).map{ case (a,b) => a }.toSeq.length != cases.map{ case (a,b,c) => a }.length) 
          {
         //   println("match")
            throw Illtyped
          }


      
           
 
            val typeOfCases = cases.map(thisCase => 
              getType(thisCase._3,env +(thisCase._2 ->constructors(typename(thisCase._1)).getOrElse(thisCase._1,throw Illtyped)))).distinct
            if ( typeOfCases.length != 1 ) {
             // println("match")
            throw Illtyped
          }

            else {
           //   println(typeOfCases.head)
              typeOfCases.head
            }

        }
        case _ => {
        //  println("match")
            throw Illtyped
          }
       }


      } // FILL ME IN

    
    }
}











