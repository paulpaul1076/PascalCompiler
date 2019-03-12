package backend.interpreter

import backend.Backend
import intermediate.{ICode, SymTab}
import message.{Message, MessageListener, MessageType}

class Executor extends Backend {
  /**
   * Process the intermediate code and the symbol table generated by the parser.
   * To be implemented by a compiler or an interpreter subclass.
   *
   * @param iCode  the intermediate code.
   * @param symTab the symbol table.
   */
  override def process(iCode: ICode, symTab: SymTab): Unit = {
    val startTime = System.currentTimeMillis()
    val elapsedTime = (System.currentTimeMillis() - startTime) / 1000f
    val executionCount = 0
    val runtimeErrors = 0

    sendMessage(new Message(MessageType.INTERPRETER_SUMMARY, List(executionCount, runtimeErrors, elapsedTime)))
  }

  /**
   * Add listener to the listener list.
   *
   * @param listener listener to be added.
   */
  override def addMessageListener(listener: MessageListener): Unit = ???

  /**
   * Remove message listener from the listener list.
   *
   * @param listener listener to be removed.
   */
  override def removeMessageListener(listener: MessageListener): Unit = ???

  /**
   * Nofity listeners after setting the message.
   *
   * @param message message the message to set.
   */
  override def sendMessage(message: Message): Unit = ???
}
