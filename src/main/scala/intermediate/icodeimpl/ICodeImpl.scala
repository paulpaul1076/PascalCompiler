package intermediate.icodeimpl

import intermediate.{ICode, ICodeNode}

class ICodeImpl extends ICode {

  private var root: ICodeNode = _

  /**
    * Set and return the root node.
    *
    * @param node the node to set as root.
    * @return the root node.
    */
  override def setRoot(node: ICodeNode): ICodeNode = {
    root = node
    root
  }

  /**
    * Get the root node.
    *
    * @return the root node.
    */
  override def getRoot: ICodeNode = root
}
