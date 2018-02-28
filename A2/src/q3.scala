import scala.io.Source
import scala.util.parsing.combinator._

/**
  * A regex parser for the WML Parser. This parser creates an AST.
  * Parse with node program as root to get complete program.
  * @author Kevin Laframboise - 260687529
  */
class WMLParser extends RegexParsers {

  /* Constant terminal tokens */
  val Tstart = "{{"
  val Tend = "}}"
  val Vstart = "{{{"
  val Vend = "}}}"
  val Dstart = "{'"
  val Dend = "'}"
  val Pipe = "|"
  val Pipes = "||"

  /* Non-constant terminal tokens*/
  val Outertext = "^((?!\\{\\{)(?!\\{')[\\s\\S])+".r
  val Inneritext = "^((?!\\{\\{)(?!\\{')(?!\\|)(?!\\}\\})[\\s\\S])+".r
  val Innerdtext = "^((?!\\{\\{)(?!\\{')(?!\\|)(?!'\\})[\\s\\S])+".r
  val Bodytext = "^((?!\\{\\{)(?!\\{')(?!'\\})[\\s\\S])+".r
  val Vname = "^((?!\\|)(?!\\}\\}\\})[\\s\\S])+".r

  /* Non-terminal tokens*/
  def program : Parser[ASTNode] = rep(Outertext | invoke | define) ^^ {
    case Nil => ASTProgram(Nil) //no program
    case list => anyListHelper(list, Nil, ASTOuterText(_), ASTProgram(_)) //get children
  }

  def invoke : Parser[ASTNode] = Tstart ~ itext ~ targs ~ Tend ^^ {
    case _ ~ i ~ t ~ _ => ASTInvoke(List(i, t)) //create invoke with itext and targs children
  }

  def targs : Parser[ASTNode] = rep(Pipe ~ opt(itext) ^^ {
    case p ~ None => (p, null)  //case pipe then empty string
    case p ~ Some(i) => (p, i)  //case pipe then itext
  }) ^^ {
    case Nil => ASTTArgs(Nil) //case no args
    case list => {
      val children = list.map((x) => x._2)  //add all itext to list of children
      ASTTArgs(children)
    }
  }

  def itext : Parser[ASTNode] = rep(Inneritext | tvar | invoke | define) ^^ {
    case Nil => ASTIText(Nil) //case empty itext
    case list => anyListHelper(list, Nil, ASTInnerIText(_), ASTIText(_))  //get children
  }

  def tvar : Parser[ASTNode] = Vstart ~ Vname ~ opt(Pipe ~ itext) ~ Vend ^^ {
    case _ ~ v ~ None ~ _ => ASTTVar(List(ASTVName(v))) //case no itext, but take vname

    case _ ~ v ~ Some(Pipe ~ i) ~ _ => ASTTVar(List(ASTVName(v), i)) //create tvar with vname and itext as children
  }

  def define : Parser[ASTNode] = Dstart ~ dtextn ~ dparams ~ Pipes ~ dtextb ~ Dend ^^ {
    case _ ~ dtn ~ dp ~ _ ~ dtb ~ _ => ASTDefine(List(dtn, dp, dtb))  //create define with texts and params as children
  }

  def dtextn : Parser[ASTNode] = rep(Innerdtext | invoke | define | tvar) ^^ {
    case Nil => ASTDTextN(Nil)  //case empty dtextn
    case list => anyListHelper(list, Nil, ASTInnerDText(_), ASTDTextN(_)) //get children
  }

  def dparams : Parser[ASTNode] = rep(Pipe ~ dtextp) ^^ {
    case Nil => ASTDParams(Nil) //case empty dparams
    case list => {
      val children = list.map((x) => x._2)  //get all dtextp and add as children to dparams
      ASTDParams(children)
    }
  }

  def dtextp : Parser[ASTNode] = rep1(Innerdtext | invoke | define | tvar) ^^ {
    case Nil => ASTDTextP(Nil)  //case empty dtextp
    case list => anyListHelper(list, Nil, ASTInnerDText(_), ASTDTextP(_)) //get children
  }

  def dtextb : Parser[ASTNode] = rep(Bodytext | invoke | define | tvar) ^^ {
    case Nil => ASTDTextB(Nil)  //case empty dtextb
    case list => anyListHelper(list, Nil, ASTBodyText(_), ASTDTextB(_)) //get children
  }

  /**
    * Used for building nodes for tokens that are composed of a list of only one type of terminal token and one or more
    * types of non-terminal tokens.
    * @param list list of tokens as provided by the rep() function
    * @param children list of children of the node being built
    * @param termCons constructor of the terminal token
    * @param nonTermCons constructor of the node being built
    * @return an ASTNode of the same type as nonTermCons
    */
  def anyListHelper(list: List[Any], children: List[ASTNode], termCons: (String) => ASTNode,
                    nonTermCons: (List[ASTNode]) => ASTNode): ASTNode = {
    if(list.isEmpty) nonTermCons(children)  //base case
    else {
      list.head match {
          //A string must be the terminal token of the type passed in termCons
        case s: String => anyListHelper(list.tail, children :+ termCons(s), termCons, nonTermCons)
          //An ASTNode, just add to list of children
        case n: ASTNode => anyListHelper(list.tail, children :+ n, termCons, nonTermCons)
      }
    }
  }
}
object q3 {
  def main(args: Array[String]): Unit = {
    if(args.length != 1) {
      println("Usage: WMLParser filename")
      System.exit(0)
    }
    val code = Source.fromFile(args(0)).mkString
    val p = new WMLParser()
    println(p.parseAll(p.program, code).get.toString)
  }
}