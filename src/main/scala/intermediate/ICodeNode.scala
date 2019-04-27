package intermediate

import java.util

/**
  * The interface for a node of the intermediate code.
  */
trait ICodeNode {

  /**
    * Getter.
    *
    * @return this node's type.
    */
  def getType: ICodeNodeType

  /**
    * Getter.
    *
    * @return the parent of the current node.
    */
  def getParent: ICodeNode

  /**
    * Adds a child node.
    *
    * @param node the child node to be added.
    * @return the child node.
    */
  def addChild(node: ICodeNode): ICodeNode

  /**
    * Getter.
    *
    * @return list of nodes.
    */
  def getChildren: util.ArrayList[ICodeNode]

  /**
    * Set this node's attribute.
    *
    * @param key   the attribute key.
    * @param value the attribute value.
    */
  def setAttribute(key: ICodeKey, value: Any): Unit

  /**
    * Getter.
    *
    * @param key the attribute key.
    * @return the attribute value.
    */
  def getAttribute(key: ICodeKey): Any

  /**
    * Make a copy of this node.
    *
    * @return the copy.
    */
  def copy(): ICodeNode

  def setTypeSpec(typeSpec: TypeSpec): Unit

  def getTypeSpec: TypeSpec
}
