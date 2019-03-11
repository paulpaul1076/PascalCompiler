package message

import java.util

/**
  * Message handler classes that everything that has to do with messages gets delegated to.
  */
class MessageHandler {
  /**
    * Current message.
    */
  private var message: Message = _

  /**
    * The list of listeners.
    */
  private val listeners = new util.ArrayList[MessageListener]

  /**
    * Add this listener to the list of listeners.
    * @param listener listener to be added.
    */
  def addMessageListener(listener: MessageListener): Unit = {
    listeners.add(listener)
  }

  /**
    * Remove this listener from the list of listeners.
    * @param listener listener to be removed.
    */
  def removeMessageListener(listener: MessageListener): Unit = {
    listeners.remove(listener)
  }

  /**
    * Notify listeners after setting the message.
    * @param message message the message to set.
    */
  def sendMessage(message: Message): Unit = {
    this.message = message
    notifyListeners()
  }

  // TODO: The method below is kind of useless? Remove it maybe?
  /**
    * Notify each listener in the listener list by calling the listener's messageReceived() method.
    *
    */
  private def notifyListeners() : Unit = {
    listeners.forEach(listener => listener.messageReceived(message))
  }
}
