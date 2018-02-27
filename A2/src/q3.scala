import scala.util.parsing.combinator._

class WMLParser extends RegexParsers {

  val Tstart = "{{"
  val Tend = "}}"
  val Vstart = "{{{"
  val Vend = "}}}"
  val Dstart = "{'"
  val Dend = "'}"
  val Pipe = "|"
  val Pipes = "||"
  val Outertext = "^((?!\\{\\{)(?!\\{')[\\s\\S])+".r
  val Inneritext = "^((?!\\{\\{)(?!\\{')(?!\\|)(?!\\}\\})[\\s\\S])+".r
  val Innerdtext = "^((?!\\{\\{)(?!\\{')(?!\\|)(?!'\\})[\\s\\S])+".r
  val Bodytext = "^((?!\\{\\{)(?!\\{')(?!'\\})[\\s\\S])+".r
  val Vname = "^((?!\\|)(?!\\}\\}\\})[\\s\\S])+".r

  def program : Parser[ASTNode] = rep(Outertext | invoke | define) ^^ {
    case Nil => ASTProgram(Nil)
    case list => anyListHelper(list, Nil, ASTOuterText(_), ASTProgram(_))
  }

  def invoke : Parser[ASTNode] = Tstart ~ itext ~ targs ~ Tend ^^ {
    case _ ~ i ~ t ~ _ => ASTInvoke(List(i, t))
  }

  def targs : Parser[ASTNode] = rep(Pipe ~ opt(itext) ^^ {
    case p ~ None => (p, null)
    case p ~ Some(i) => (p, i)
  }) ^^ {
    case Nil => ASTTArgs(Nil)
    case list => {
      val children = list.map((x) => x._2)
      ASTTArgs(children)
    }
  }

  def itext : Parser[ASTNode] = rep(Inneritext | tvar | invoke | define) ^^ {
    case Nil => ASTIText(Nil)
    case list => anyListHelper(list, Nil, ASTInnerIText(_), ASTIText(_))
  }

  def tvar : Parser[ASTNode] = Vstart ~ Vname ~ opt(Pipe ~ itext) ~ Vend ^^ {
    case _ ~ _ ~ None ~ _ => ASTTVar(Nil)
    case _ ~ _ ~ Some(Pipe ~ i) ~ _ => ASTTVar(List(i))
  }

  def define : Parser[ASTNode] = Dstart ~ dtextn ~ dparams ~ Pipes ~ dtextb ~ Dend ^^ {
    case _ ~ dtn ~ dp ~ _ ~ dtb ~ _ => ASTDefine(List(dtn, dp, dtb))
  }

  def dtextn : Parser[ASTNode] = rep(Innerdtext | invoke | define | tvar) ^^ {
    case Nil => ASTDTextN(Nil)
    case list => anyListHelper(list, Nil, ASTInnerDText(_), ASTDTextN(_))
  }

  def dparams : Parser[ASTNode] = rep(Pipe ~ dtextp) ^^ {
    case Nil => ASTDParams(Nil)
    case list => {
      val children = list.map((x) => x._2)
      ASTDParams(children)
    }
  }

  def dtextp : Parser[ASTNode] = rep1(Innerdtext | invoke | define | tvar) ^^ {
    case Nil => ASTDTextP(Nil)
    case list => anyListHelper(list, Nil, ASTInnerDText(_), ASTDTextP(_))
  }

  def dtextb : Parser[ASTNode] = rep(Bodytext | invoke | define | tvar) ^^ {
    case Nil => ASTDTextB(Nil)
    case list => anyListHelper(list, Nil, ASTBodyText(_), ASTDTextB(_))
  }

  /**
    * Used for building nodes for tokens that are composed of a list of only one type of terminal token and one or more
    * type of non-terminal tokens.
    * @param list list of tokens as provided by the rep() function
    * @param children list of children of the node being built
    * @param termCons constructor of the terminal token
    * @param nonTermCons constructor of the node being built
    * @return an ASTNode of the same type as nonTermCons
    */
  def anyListHelper(list: List[Any], children: List[ASTNode], termCons: (String) => ASTNode,
                    nonTermCons: (List[ASTNode]) => ASTNode): ASTNode = {
    if(list.isEmpty) nonTermCons(children)
    else {
      list(0) match {
        case s: String => anyListHelper(list.tail, children :+ termCons(s), termCons, nonTermCons)
        case n: ASTNode => anyListHelper(list.tail, children :+ n, termCons, nonTermCons)
      }
    }
  }
}
object q3 {
  def main(args: Array[String]): Unit = {
    val p = new WMLParser()
    println(p.parseAll(p.program, "abc def {{ xxx | bar uuuu }}").get.toString)
  }
}