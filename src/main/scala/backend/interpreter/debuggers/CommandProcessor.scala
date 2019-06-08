package backend.interpreter.debuggers


import java.util

import backend.interpreter.{ActivationRecord, Cell, Debugger}
import frontend.pascal.PascalTokenType
import intermediate.SymTab
import intermediate.symtabimpl.SymTabKeyImpl
import message.{Message, MessageType}

import scala.collection.JavaConverters._

/**
  * Logic implementation of all command line debugger commands.
  * @param debuggerr debugger.
  */
class CommandProcessor(debuggerr: Debugger) {
  private var debugger: Debugger = debuggerr // the debugger
  private var stepping: Boolean = true // true when single stepping

  /**
    * Process a message from the back end.
    * @param message the message.
    */
  def processMessage(message: Message): Unit = {
    val `type` = message.messageType

    `type` match {
      case MessageType.SOURCE_LINE =>
        val lineNumber = message.body.asInstanceOf[Integer]

        if (stepping) {
          debugger.atStatement(lineNumber)
          debugger.readCommands()
        } else {
          if (debugger.isBreakpoint(lineNumber)) {
            debugger.atBreakpoint(lineNumber)
            debugger.readCommands()
          }
        }
      case MessageType.FETCH =>
        val body = message.body.asInstanceOf[List[Any]]
        val variableName = body(1).asInstanceOf[String].toLowerCase

        if (debugger.isWatchpoint(variableName)) {
          val lineNumber = body(0).asInstanceOf[Integer]
          val value = body(2)

          debugger.atWatchpointValue(lineNumber, variableName, value)
        }
      case MessageType.ASSIGN =>
        val body = message.body.asInstanceOf[List[Any]]
        val variableName = body(1).asInstanceOf[String].toLowerCase

        if (debugger.isWatchpoint(variableName)) {
          val lineNumber = body(0).asInstanceOf[Integer]
          val value = body(2)

          debugger.atWatchpointAssignment(lineNumber, variableName, value)
        }
      case MessageType.CALL =>
        val body = message.body.asInstanceOf[List[Any]]
        val lineNumber = body(0).asInstanceOf[Integer]
        val routineName = body(1).asInstanceOf[String]

        debugger.callRoutine(lineNumber, routineName)
      case MessageType.RETURN =>
        val body = message.body.asInstanceOf[List[Any]]
        val lineNumber = body(0).asInstanceOf[Integer]
        val routineName = body(1).asInstanceOf[String]

        debugger.returnRoutine(lineNumber, routineName)
      case MessageType.RUNTIME_ERROR =>
        val body = message.body.asInstanceOf[List[Any]]
        val errorMessage = body(0).asInstanceOf[String]
        val lineNumber = body(1).asInstanceOf[Integer]

        debugger.runtimeError(errorMessage, lineNumber)
      case _ =>
    }
  }

  def parseCommand(): Boolean = {
    var anotherCommand = true

    // Parse a command.
    try {
      debugger.nextToken()
      val command = debugger.getWord("Command expected.")
      anotherCommand = executeCommand(command)
    } catch {
      case ex: Exception =>
        debugger.commandError(ex.getMessage)
    }

    // Skip to the next command.
    try {
      debugger.skipToNextCommand()
    } catch {
      case ex: Exception =>
        debugger.commandError(ex.getMessage)
    }

    return anotherCommand
  }

  def executeCommand(command: String): Boolean = {
    stepping = false

    if (command.equals("step")) {
      stepping = true
      checkForSemicolon()
      return false
    }

    if (command.equals("break")) {
      val lineNumber = debugger.getInteger("Line number expected.")
      checkForSemicolon()
      debugger.setBreakpoint(lineNumber.asInstanceOf[Integer])
      return true
    }

    if (command.equals("unbreak")) {
      val lineNumber = debugger.getInteger("Line number expected.")
      checkForSemicolon()
      debugger.unsetBreakpoint(lineNumber.asInstanceOf[Integer])
      return true
    }

    if (command.equals("watch")) {
      val name = debugger.getWord("Variable name expected.")
      checkForSemicolon()
      debugger.setWatchpoint(name)
      return true
    }

    if (command.equals("unwatch")) {
      val name = debugger.getWord("Variable name expected.")
      checkForSemicolon()
      debugger.unsetWatchpoint(name)
      return true
    }

    if (command.equals("stack")) {
      checkForSemicolon()
      stack()
      return true
    }

    if (command.equals("show")) {
      show()
      return true
    }

    if (command.equals("assign")) {
      assign()
      return true
    }

    if (command.equals("go")) {
      checkForSemicolon()
      return false
    }

    if (command.equals("quit")) {
      checkForSemicolon()
      debugger.quit()
    }

    throw new Exception("Invalid command: '" + command + "'.")
  }

  def stack(): Unit = {
    val callStack = new util.ArrayList[Any]

    // Loop over the activation record on the runtime stack
    // starting at the top of the stack.

    val runtimeStack = debugger.getRuntimeStack
    val arList = runtimeStack.records()

    for (i <- arList.size() - 1 to 0 by -1) {
      val ar = arList.get(i)
      val routineId = ar.getRoutineId

      // Add the symbol table entry of the procedure or function.
      callStack.add(routineId)

      // Create and add a name-value pair for each local variable.
      for (name <- ar.getAllNames.asScala) {
        val value = ar.getCell(name).getValue
        callStack.add(new NameValuePair(name, value))
      }
    }

    // Display the call stack.
    debugger.displayCallStack(callStack)
  }

  def show(): Unit = {
    val pair = createCellTypePair()
    val cell = pair.getCell

    checkForSemicolon()
    debugger.displayValue(NameValuePair.valueString(cell.getValue))
  }

  def assign(): Unit = {
    val pair = createCellTypePair()
    val newValue = debugger.getValue("Invalid value.")

    checkForSemicolon()
    pair.setValue(newValue)
  }

  def createCellTypePair(): CellTypePair = {
    val runtimeStack = debugger.getRuntimeStack
    val currentLevel = runtimeStack.currentNestingLevel
    var ar: ActivationRecord = null
    var cell: Cell = null

    // Parse the variable name.
    val variableName = debugger.getWord("Variable name expected.")

    // Find the variable's cell in the call stack.
    var level = currentLevel
    while (cell == null && level > 0) {
      ar = runtimeStack.getTopmost(level)
      cell = ar.getCell(variableName)
      level -= 1
    }

    if (cell == null) {
      throw new Exception("Undeclared variable name '" + variableName + "'.")
    }

    // VAR parameter.
    if (cell.getValue.isInstanceOf[Cell]) {
      cell = cell.getValue.asInstanceOf[Cell]
    }

    // Find the variable's symbol table entry.
    val symTab = ar.getRoutineId.getAttribute(SymTabKeyImpl.ROUTINE_SYMTAB).asInstanceOf[SymTab]
    val id = symTab.lookup(variableName)

    return new CellTypePair(id.getTypeSpec, cell, debugger)
  }

  /**
    * Verify that a command ends with a semicolon.
    */
  def checkForSemicolon(): Unit = {
    if (debugger.currentToken().getTokenType != PascalTokenType.SEMICOLON) {
      throw new Exception("Invalid command syntax.")
    }
  }
}
