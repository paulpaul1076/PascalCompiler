package intermediate

/**
  * The framework interface that represents the intermediate code.
  */
trait ICode {
  /**
    * Set and return the root node.
    *
    * @param node the node to set as root.
    * @return the root node.
    */
  def setRoot(node: ICodeNode): ICodeNode

  /**
    * Get the root node.
    *
    * @return the root node.
    */
  def getRoot: ICodeNode
}
