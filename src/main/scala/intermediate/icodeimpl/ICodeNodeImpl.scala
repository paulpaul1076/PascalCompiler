package intermediate.icodeimpl

import java.util

import intermediate._

/**
  * Implementation of ICodeNode.
  *
  * @param nodeType the type of ndoe.
  */
class ICodeNodeImpl(private val nodeType: ICodeNodeType) extends util.HashMap[ICodeKey, Any] with ICodeNode {

  /**
    * Node type.
    */
  private var parent: ICodeNode = _

  /**
    * Children array list.
    */
  private var children: util.ArrayList[ICodeNode] = new util.ArrayList[ICodeNode]()

  private var typeSpec: TypeSpec = _

  /**
    * Getter.
    *
    * @return this node's type.
    */
  override def getType: ICodeNodeType = nodeType

  /**
    * Getter.
    *
    * @return the parent of the current node.
    */
  override def getParent: ICodeNode = parent

  /**
    * Adds a child node.
    *
    * @param node the child node to be added.
    * @return the child node.
    */
  override def addChild(node: ICodeNode): ICodeNode = {
    if (node != null) {
      children.add(node)
      node.asInstanceOf[ICodeNodeImpl].parent = this
    }
    node
  }

  /**
    * Getter.
    *
    * @return list of nodes.
    */
  override def getChildren: util.ArrayList[ICodeNode] = children

  /**
    * Set this node's attribute.
    *
    * @param key   the attribute key.
    * @param value the attribute value.
    */
  override def setAttribute(key: ICodeKey, value: Any): Unit = {
    put(key, value)
  }

  /**
    * Getter.
    *
    * @param key the attribute key.
    * @return the attribute value.
    */
  override def getAttribute(key: ICodeKey): Any = get(key)

  /**
    * Make a copy of this node **without parent or children**.
    *
    * @return the copy.
    */
  override def copy(): ICodeNode = {
    val copy = ICodeFactory.createICodeNode(nodeType)
    entrySet().stream().forEach(entry => {
      copy.setAttribute(entry.getKey, entry.getValue)
    })
    copy
  }

  /**
    * String representation of this.
    *
    * @return string representation.
    */
  override def toString: String = nodeType.toString

  override def getTypeSpec: TypeSpec = {
    typeSpec
  }

  override def setTypeSpec(typeSpec: TypeSpec): Unit = {
    this.typeSpec = typeSpec
  }
}
