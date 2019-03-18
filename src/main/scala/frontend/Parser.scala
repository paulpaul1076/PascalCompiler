package frontend

import intermediate.{ICode, SymTab, SymTabFactory, SymTabStack}
import message.{Message, MessageHandler, MessageListener, MessageProducer}

/**
  * Parser skeleton.
  *
  * @param scanner scanner.
  */
abstract class Parser(protected val scanner: Scanner) extends MessageProducer {

  /**
    * Intermediate code.
    */
  protected var iCode: ICode = _

  /**
    * Method to be overridden by a specific parser.
    */
  def parse(): Unit

  /**
    * Get the number of errors encountered during parsing.
    *
    * @return number of errors.
    */
  def getErrorCount: Int

  /**
    * Get the current token.
    *
    * @return current token.
    */
  def currentToken(): Token = {
    scanner.currentToken()
  }

  /**
    * Get the next token.
    *
    * @return next token.
    */
  def nextToken(): Token = {
    scanner.nextToken()
  }

  /**
    * Getter for the intermediate code.
    *
    * @return iCode.
    */
  def getICode: ICode = iCode

  /**
    * Getter for the symbol table.
    *
    * @return the symbol table.
    */
  def getSymTab: SymTab = Parser.symTab

  // ---------- Message producing stuff

  /**
    * Delegates its work to MessageHandler to add a new listener.
    *
    * @param listener listener to be added.
    */
  override def addMessageListener(listener: MessageListener): Unit = {
    Parser.messageHandler.addMessageListener(listener)
  }

  /**
    * Delegates its work to MessageHandler to add a new listener.
    *
    * @param listener listener to be removed.
    */
  override def removeMessageListener(listener: MessageListener): Unit = {
    Parser.messageHandler.removeMessageListener(listener)
  }

  /**
    * Delegates its work to MessageHandler to add a new listener.
    *
    * @param message message the message to set.
    */
  override def sendMessage(message: Message): Unit = {
    Parser.messageHandler.sendMessage(message)
  }
}

/**
  * Companion object.
  */
object Parser {
  /**
    * Symbol table.
    */
  var symTab: SymTab = _ // TODO: should it be global like this?

  /**
    * Symbol table stack.
    */
  var symTabStack: SymTabStack = SymTabFactory.createSymTabStack() // TODO: should it be global like this?

  /**
    * A delegate instance that handles message sending.
    */
  val messageHandler: MessageHandler = new MessageHandler // TODO: should it be global like this?
}
