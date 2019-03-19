package intermediate

/**
  * A factory for creating objects that implement the intermediate code.
  */
object ICodeFactory {

  /**
    * Create and return an intermediate code implementation.
    *
    * @return the intermediate code implementation.
    */
  def createICode(): ICode = {
    new ICodeImpl()
  }

  /**
    * Create and return a node implementation.
    *
    * @param nodeType the node type.
    * @return the node implementation.
    */
  def createICodeNode(nodeType: ICodeNodeType): ICodeNode = {
    new ICodeNodeImpl(nodeType)
  }
}
