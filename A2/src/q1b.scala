import scala.util.parsing.combinator._

class WMLParser extends RegexParsers {

  val Tstart = "^\\{\\{(?!\\{)".r
  val Tend = "^\\}\\}(?!\\})".r
  val Vstart = "^\\{\\{\\{".r
  val Vend = "^\\}\\}\\}".r
  val Dstart = "^\\{'".r
  val Dend = "^'\\}".r
  val Pipe = "^\\|(?!\\|)".r
  val Pipes = "^\\|\\|".r
  val Outertext = "^((?!\\{\\{)(?!\\{')[\\s\\S])*$".r
  val Inneritext = "^((?!\\{\\{)(?!\\{')(?!\\|)(?!\\}\\})[\\s\\S])*$".r
  val Innerdtext = "^((?!\\{\\{)(?!\\{')(?!\\|)(?!'\\})[\\s\\S])*$".r
  val Bodytext = "^((?!\\{\\{)(?!\\{')(?!'\\})[\\s\\S])*$".r
  val Vname = "^((?!\\|)(?!\\}\\}\\})[\\s\\S])*$".r

  def program : Parser[Any] = rep(Outertext | invoke | define)
  def invoke : Parser[Any] = Tstart ~ itext ~ targs ~ Tend
  def targs : Parser[Any] = rep(Pipe ~ opt(itext))
  def itext : Parser[Any] = rep(Inneritext | tvar | invoke | define)
  def tvar : Parser[Any] = Vstart ~ Vname ~ opt(Pipe ~ itext) ~ Vend

  def define : Parser[Any] = Dstart ~ dtextn ~ dparams ~ Pipes ~ dtextb ~ Dend
  def dtextn : Parser[Any] = rep(Innerdtext | invoke | define | tvar)
  def dparams : Parser[Any] = rep(Pipe ~ dtextp)
  def dtextp : Parser[Any] = rep1(Innerdtext | invoke | define | tvar)
  def dtextb : Parser[Any] = rep(Bodytext | invoke | define | tvar)

}
