package intermediate.icodeimpl

import intermediate.ICodeKey

/**
 * Attribute keys for an intermediate code node.
 */
class ICodeKeyImpl(val name: String) extends ICodeKey {
  override def toString: String = name
}

/**
 * Constants.
 */
object ICodeKeyImpl {
  val LINE = new ICodeKeyImpl("line")
  val ID = new ICodeKeyImpl("id")
  val VALUE = new ICodeKeyImpl("value")
}
