/**
  * Top level class representing all types of AST Nodes
  * @author Kevin Laframboise - 260687529
  */
abstract class ASTNode

/**
  * Class representing a terminal token
  * @param value value at the node (i.e. variable name or text)
  */
abstract class ASTNodeTerminal(val value: String) extends ASTNode

/**
  * Class representing a non-terminal token
  * Non-terminal tokens have children, as opposed to terminal nodes
  * @param children list of children
  */
abstract class ASTNodeNonTerminal(val children: List[ASTNode]) extends ASTNode {

  /**
    * @return a string representation of the list of children
    */
  override def toString: String = {
    if(children == null) ""
    else {
      val childrenStrings = children.map(x => x.toString) //get string for every children
      childrenStrings.mkString(" ") //convert list of string into single string
    }

  }

}

/*                           TERMINAL NODES                             */
case class ASTOuterText(val text: String) extends ASTNodeTerminal(text) {
  override def toString: String = "OUTERTEXT"
}
case class ASTInnerIText(val text: String) extends ASTNodeTerminal(text) {
  override def toString: String = "INNERITEXT"
}
case class ASTInnerDText(val text: String) extends ASTNodeTerminal(text) {
  override def toString: String = "INNERDTEXT"
}
case class ASTBodyText(val text: String) extends ASTNodeTerminal(text) {
  override def toString: String = "BODYTEXT"
}
case class ASTVName(val name: String) extends ASTNodeTerminal(name) {
  override def toString: String = "VNAME"
}

/*                                      NON-TERMINAL NODES                                     */
case class ASTProgram(override val children: List[ASTNode]) extends ASTNodeNonTerminal(children) {
  override def toString: String = "PROGRAM ( " + super.toString + " )"
}
case class ASTInvoke(override val children: List[ASTNode]) extends ASTNodeNonTerminal(children) {
  override def toString: String = "INVOKE ( " + super.toString + " )"
}
case class ASTTArgs(override val children: List[ASTNode]) extends ASTNodeNonTerminal(children) {
  override def toString: String =  "TARGS ( " + super.toString + " )"
}
case class ASTIText(override val children: List[ASTNode]) extends ASTNodeNonTerminal(children) {
  override def toString: String = "ITEXT ( " + super.toString + " )"
}
case class ASTTVar(override val children: List[ASTNode]) extends ASTNodeNonTerminal(children) {
  override def toString: String = "TVAR ( " + super.toString + " )"
}
case class ASTDefine(override val children: List[ASTNode]) extends ASTNodeNonTerminal(children) {
  override def toString: String = "DEFINE ( " + super.toString + " )"
}
case class ASTDTextN(override val children: List[ASTNode]) extends ASTNodeNonTerminal(children) {
  override def toString: String = "DTEXTN ( " + super.toString + " )"
}
case class ASTDParams(override val children: List[ASTNode]) extends ASTNodeNonTerminal(children) {
  override def toString: String = "DPARAMS ( " + super.toString + " )"
}
case class ASTDTextP(override val children: List[ASTNode]) extends ASTNodeNonTerminal(children) {
  override def toString: String = "DTEXTP ( " + super.toString + " )"
}
case class ASTDTextB(override val children: List[ASTNode]) extends ASTNodeNonTerminal(children) {
  override def toString: String = "DTEXTB ( " + super.toString + " )"
}
