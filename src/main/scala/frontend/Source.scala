package frontend

import java.io.{BufferedReader, IOException}

import message.{Message, MessageListener, MessageProducer}

// Open questions
// 1) When can (currentPos == Source.BEFORE_NEW_LINE) happen in currentChar()?
// 2) Isn't peekChar exhibiting wrong behavior when we are at EOL?

/**
  * The framework class that represents the source program.
  *
  * @param reader reader of the program.
  */
class Source(private val reader: BufferedReader) extends MessageProducer {
  /**
    * Source line.
    */
  private var line: String = _

  /**
    * Current source line number.
    */
  private var lineNum: Int = 0

  /**
    * Current source line pos.
    */
  private var currentPos: Int = Source.FIRST_TIME_READING_FILE

  def getLineNum: Int = {
    lineNum
  }

  def getPosition: Int = {
    currentPos
  }

  /**
    * Returns the current char.
    *
    * @return current char.
    */
  def currentChar(): Char = {
    if (currentPos == Source.FIRST_TIME_READING_FILE) {
      readLine()
      nextChar()
    } else if (line == null) {
      Source.EOF
    } else if ((currentPos == Source.BEFORE_NEW_LINE) || (currentPos == line.length)) {
      Source.EOL
    } else {
      line.charAt(currentPos)
    }
  }

  /**
    * Reads the next source line
    *
    * @throws java.io.IOException when there's an error while reading from source file
    */
  @throws(classOf[IOException])
  private def readLine(): Unit = {
    line = reader.readLine() // null when at the end of the source
    currentPos = Source.BEFORE_NEW_LINE

    if (line != null) {
      lineNum += 1
    }

    sendMessage(new Message(SOURCE_LINE, {lineNum, line})) // TODO: Figure this out
  }

  /**
    * Look and consume the next char.
    *
    * @return next char.
    */
  def nextChar(): Char = {
    currentPos += 1
    currentChar()
  }

  /**
    * Look at the next char without consuming it.
    *
    * @return next char.
    */
  def peekChar(): Char = {
    currentChar()
    val nextPos = currentPos + 1
    if (line == null) {
      Source.EOF
    } else if (nextPos < line.length) {
      line.charAt(nextPos)
    } else {
      Source.EOL
    }
  }

  /**
    * Close this source reader.
    */
  def close(): Unit = {
    if (reader != null) {
      try {
        reader.close()
      } catch {
        case ex: IOException =>
          ex.printStackTrace()
          throw ex
      }
    }
  }

  // --------------- MessageProducer

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

/**
  * Companion object.
  */
object Source {

  /**
    * End of line.
    */
  val EOL = '\n'

  /**
    * End of file.
    */
  val EOF: Char = 0

  val FIRST_TIME_READING_FILE: Int = -2
  val BEFORE_NEW_LINE: Int = -1
}