package message

/**
  * Message producer interface to be implemented by Scanner and Source.
  */
trait MessageProducer {
  /**
    * Add listener to the listener list.
    * @param listener to be added.
    */
  def addMessageListener(listener: MessageListener) : Unit

  /**
    * Remove message listener from the listener list.
    * @param listener to be removed.
    */
  def removeMessageListener(listener: MessageListener) : Unit

  /**
    * Notify listeners after setting the message.
    * @param message the message to set.
    */
  def sendMessage(message: Message) : Unit
}
