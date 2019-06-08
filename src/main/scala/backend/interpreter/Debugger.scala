package backend.interpreter

import java.io.{BufferedReader, IOException, InputStreamReader}
import java.util

import backend.Backend
import frontend.{Scanner, Source, Token}
import frontend.pascal.{PascalScanner, PascalTokenType}
import message.{Message, MessageListener}

/**
  * Base debugger class.
  *
  * @param backend       backend.
  * @param runtimeStackk runtime stack.
  */
abstract class Debugger(backend: Backend, runtimeStackk: RuntimeStack) {

  private var runtimeStack: RuntimeStack = _
  private var breakpoints: util.HashSet[Int] = _
  private var watchpoints: util.HashSet[String] = _
  private var commandInput: Scanner = _

  def getRuntimeStack: RuntimeStack = runtimeStack

  // Constructor
  {
    runtimeStack = runtimeStackk
    backend.addMessageListener(new BackendMessageListener())

    breakpoints = new util.HashSet[Int]()
    watchpoints = new util.HashSet[String]()

    // Create the command input from the standard input.
    try {
      commandInput = new PascalScanner(
        new Source(
          new BufferedReader(
            new InputStreamReader(System.in)
          )
        )
      )
    } catch {
      case e: IOException =>
    }
  }

  /**
    * Listener for backend messages.
    */
  private class BackendMessageListener extends MessageListener {

    /**
      * Called by the back end whenever it produces a message.
      *
      * @param message message the message that was sent.
      */
    override def messageReceived(message: Message): Unit = {
      processMessage(message)
    }
  }

  /**
    * Read the debugger commands.
    */
  def readCommands(): Unit = {
    do {
      promptForCommand()
    } while (parseCommand())
  }

  /**
    * Return the current token from the command input.
    *
    * @return the token.
    */
  def currentToken(): Token = {
    return commandInput.currentToken()
  }

  /**
    * Return the next token from the command input.
    *
    * @return the token.
    */
  def nextToken(): Token = {
    return commandInput.nextToken()
  }

  /**
    * Get the next word token from the command input.
    *
    * @param errorMessage the error message if an exception is thrown.
    * @return the text of the word token.
    */
  def getWord(errorMessage: String): String = {
    val token = currentToken()
    val `type` = token.getTokenType

    if (`type` == PascalTokenType.IDENTIFIER) {
      val word = token.getText.toLowerCase
      nextToken()
      return word
    } else {
      throw new Exception(errorMessage)
    }
  }

  /**
    * Get the next integer constant token from the command input.
    *
    * @param errorMessage the error message if an exception is thrown.
    * @return the constant integer value.
    */
  def getInteger(errorMessage: String): Int = {
    val token = currentToken()
    val `type` = token.getTokenType

    if (`type` == PascalTokenType.INTEGER) {
      val value = token.getValue.asInstanceOf[Int]
      nextToken()
      return value
    } else {
      throw new Exception(errorMessage)
    }
  }

  /**
    * Get the next constant value token from the command input.
    *
    * @param errorMessage the error message if an exception is thrown.
    * @return the constant value.
    */
  def getValue(errorMessage: String): Any = {
    var token = currentToken()
    var tokenType = token.getTokenType
    var sign = false
    var minus = false

    // Unary plus or minus sign.
    if (tokenType == PascalTokenType.MINUS || tokenType == PascalTokenType.PLUS) {
      sign = true
      minus = tokenType == PascalTokenType.MINUS
      token = nextToken()
      tokenType = token.getTokenType
    }

    tokenType.asInstanceOf[PascalTokenType] match {
      case PascalTokenType.INTEGER =>
        val value = token.getValue.asInstanceOf[Int]
        nextToken()
        return if (minus) -value else value
      case PascalTokenType.REAL =>
        val value = token.getValue.asInstanceOf[java.lang.Float]
        nextToken()
        return if (minus) -value else value
      case PascalTokenType.STRING =>
        if (sign) {
          throw new Exception(errorMessage)
        } else {
          val value = token.getValue.asInstanceOf[String]
          nextToken()
          return value.charAt(0)
        }
      case PascalTokenType.IDENTIFIER =>
        if (sign) {
          throw new Exception(errorMessage)
        } else {
          val name = token.getText
          nextToken()

          if (name.equalsIgnoreCase("true")) {
            return true
          } else if (name.equalsIgnoreCase("false")) {
            return false
          } else {
            throw new Exception(errorMessage)
          }
        }
      case _ => throw new Exception(errorMessage)
    }
  }

  /**
    * Skip the rest of this command input line.
    */
  def skipToNextCommand(): Unit = {
    commandInput.skipToNextLine()
  }

  /////////////////////////////////////
  // Breakpoint and watchpoint methods.
  /////////////////////////////////////

  /**
    * Set a breakpoint at a source line.
    *
    * @param lineNumber the source line number.
    */
  def setBreakpoint(lineNumber: Int): Unit = {
    breakpoints.add(lineNumber)
  }

  /**
    * Remove a breakpoint at a source line number
    *
    * @param lineNumber line number to set this breakpoint at.
    */
  def unsetBreakpoint(lineNumber: Int): Unit = {
    breakpoints.remove(lineNumber)
  }

  /**
    * Check if a source line is at a breakpoint.
    *
    * @param lineNumber the source line number.
    * @return true if at a breakpoint, else false.
    */
  def isBreakpoint(lineNumber: Int): Boolean = {
    return breakpoints.contains(lineNumber)
  }

  /**
    * Set a watchpoint on a variable.
    *
    * @param name the variable name.
    */
  def setWatchpoint(name: String): Unit = {
    watchpoints.add(name)
  }

  /**
    * Remove a watchpoint on a variable.
    *
    * @param name the variable name.
    */
  def unsetWatchpoint(name: String): Unit = {
    watchpoints.remove(name)
  }

  /**
    * Check if a variable is a watchpoint.
    *
    * @param name name of the variable.
    * @return true if a watchpoint, else false.
    */
  def isWatchpoint(name: String): Boolean = {
    return watchpoints.contains(name)
  }

  ///////////////////////////////////////////////////////////////////////////////////
  // Abstract methods, to be overriden by specific debuggers (command line, GUI, etc).
  ///////////////////////////////////////////////////////////////////////////////////

  /**
    * Process a message from the back end.
    * @param message the message.
    */
  def processMessage(message: Message): Unit

  /**
    * Display a prompt for a debugger command.
    */
  def promptForCommand(): Unit

  /**
    * Parse a debugger command.
    * @return true to parse another command immediately, else false.
    */
  def parseCommand(): Boolean

  /**
    * Process a source statement.
    * @param lineNumber the statement line number.
    */
  def atStatement(lineNumber: Int): Unit

  /**
    * Process a breakpoint at a statement.
    * @param lineNumber the statement line number.
    */
  def atBreakpoint(lineNumber: Int): Unit

  /**
    * Process the current value of a watchpoint variable.
    * @param lineNumber the current statement line number.
    * @param name the variable name.
    * @param value the variable's value.
    */
  def atWatchpointValue(lineNumber: Int, name: String, value: Any): Unit

  /**
    * Process the assignment of a new value to a watchpoint variable.
    * @param lineNumber the current statement's line number.
    * @param name the variable name.
    * @param value the new value.
    */
  def atWatchpointAssignment(lineNumber: Int, name: String, value: Any): Unit

  /**
    * Process calling a declared procedure or function.
    * @param lineNumber the current statement's line number.
    * @param routineName the routine name.
    */
  def callRoutine(lineNumber: Int, routineName: String): Unit

  /**
    * Process returning from a declared procedure or function.
    * @param lineNumber the current statement line number.
    * @param routineName the routine name.
    */
  def returnRoutine(lineNumber: Int, routineName: String): Unit

  /**
    * Display a value.
    * @param valueString the value string.
    */
  def displayValue(valueString: String): Unit

  /**
    * Display the call stack.
    * @param stack the list of elements of the call stack.
    */
  def displayCallStack(stack: util.ArrayList[Any]): Unit

  /**
    * Terminate execution of the source program.
    */
  def quit(): Unit

  /**
    * Handle a debugger command error.
    * @param errorMessage the error message.
    */
  def commandError(errorMessage: String): Unit

  /**
    * Handle a source program runtime error.
    * @param errorMessage the error message.
    * @param lineNumber the source line number where the error occurred.
    */
  def runtimeError(errorMessage: String, lineNumber: Integer): Unit

}
