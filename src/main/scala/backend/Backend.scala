package backend

import intermediate.{ICode, SymTab}
import message.{MessageHandler, MessageProducer}

/**
 * Abstract backend component, to be extended as a specific backend.
 */
abstract class Backend extends MessageProducer {

  /**
   * Symbol table.
   */
  protected var symTab: SymTab = _

  /**
   * Intermediate code.
   */
  protected var iCode: ICode = _

  /**
   * Process the intermediate code and the symbol table generated by the parser.
   * To be implemented by a compiler or an interpreter subclass.
   *
   * @param iCode  the intermediate code.
   * @param symTab the symbol table.
   */
  def process(iCode: ICode, symTab: SymTab): Unit
}

/**
 * Companion object for the backend component.
 */
object Backend {
  protected val messageHandler = new MessageHandler
}