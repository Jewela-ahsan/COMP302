import scala.util.matching.Regex

object Regex_Tests {

  val Outertext = "^((?!\\{\\{)(?!\\{')[\\s\\S])*".r
  val Inneritext = "^((?!\\{\\{)(?!\\{')(?!\\|)(?!\\}\\})[\\s\\S])*".r
  val Innerdtext = "^((?!\\{\\{)(?!\\{')(?!\\|)(?!'\\})[\\s\\S])*".r
  val Bodytext = "^((?!\\{\\{)(?!\\{')(?!'\\})[\\s\\S])*".r
  val Vname = "^((?!\\|)(?!\\}\\}\\})[\\s\\S])*".r

  def main(args: Array[String]): Unit = {
    testRegex(Outertext)
    testRegex(Inneritext)
    testRegex(Innerdtext)
    testRegex(Bodytext)
    testRegex(Vname)
  }

  def testRegex(r: Regex): Unit = {
    r.findFirstIn("abc  d'}e}}{' {{{fg{{{ hi") match {
      case None => println("none found")
      case Some(x) => println("found: " + x)
    }
  }

}
