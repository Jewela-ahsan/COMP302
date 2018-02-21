/**
* @author Kevin Laframboise - 260687529
*/
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

