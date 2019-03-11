package frontend

import message.{Message, MessageHandler, MessageListener, MessageProducer}

abstract class Parser(protected val scanner: Scanner) extends MessageProducer{
  protected var iCode: ICode = _
  abstract def parse()
  abstract def getErrorCount : Int
  def currentToken() : Token = {
    scanner.currentToken()
  }
  def nextToken() : Token = {
    scanner.nextToken()
  }

  // ---------- Message producing stuff

  /**
    * Delegates its work to MessageHandler to add a new listener.
    * @param listener listener to be added.
    */
  override def addMessageListener(listener: MessageListener): Unit = {
    Parser.messageHandler.addMessageListener(listener)
  }

  /**
    * Delegates its work to MessageHandler to add a new listener.
    * @param listener listener to be removed.
    */
  override def removeMessageListener(listener: MessageListener): Unit = {
    Parser.messageHandler.removeMessageListener(listener)
  }

  /**
    * Delegates its work to MessageHandler to add a new listener.
    * @param message message the message to set.
    */
  override def sendMessage(message: Message): Unit = {
    Parser.messageHandler.sendMessage(message)
  }
}

object Parser {
  protected var symTab: SymTab = _
  protected val messageHandler: MessageHandler = new MessageHandler
}
