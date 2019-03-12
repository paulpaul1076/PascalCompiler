package message

/**
  * This is a dto class that encapsulates the body of a message and its type.
  *
  * @param messageType type of the message.
  * @param body        message data.
  */
class Message(val messageType: MessageType.Value, val body: Any)
