package message

/**
  * All classes that subscribe to messages must implement this interface.
  * Each time a message producer produces a new message,
  * it notifies all its message listeners by calling each listener's messageReceived method.
  */
trait MessageListener {
  /**
    * Called to receive a message sent by a message producer.
    * @param message message the message that was sent.
    */
  def messageReceived(message: Message)
}
