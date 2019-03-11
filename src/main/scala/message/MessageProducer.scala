package message

/**
  * Message producer interface to be implemented by Scanner and Source.
  */
trait MessageProducer {
  /**
    * Add listener to the listener list.
    * @param listener listener to be added.
    */
  def addMessageListener(listener: MessageListener)

  /**
    * Remove message listener from the listener list.
    * @param listener listener to be removed.
    */
  def removeMessageListener(listener: MessageListener)

  /**
    * Nofity listeners after setting the message.
    * @param message message the message to set.
    */
  def sendMessage(message: Message)
}
