package backend.interpreter.debuggers

import java.util

import backend.Backend
import backend.interpreter.{Debugger, RuntimeStack}
import intermediate.SymTabEntry
import message.Message

import ide.IDEControl._

import scala.collection.JavaConverters._

/**
  * Command line debugger implementation.
  *
  * @param backend      backend.
  * @param runtimeStack runtime stack.
  */
class GUIDebugger(backend: Backend, runtimeStack: RuntimeStack) extends Debugger(backend, runtimeStack) {
  private val commandProcessor = new CommandProcessor(this)

  override def processMessage(message: Message): Unit = {
    commandProcessor.processMessage(message)
  }

  override def promptForCommand(): Unit = {
  }

  override def parseCommand(): Boolean = {
    return commandProcessor.parseCommand()
  }

  override def atStatement(lineNumber: Int): Unit = {
    println(DEBUGGER_AT_TAG + lineNumber)
  }

  override def atBreakpoint(lineNumber: Int): Unit = {
    println(DEBUGGER_BREAK_TAG + lineNumber)
  }

  override def atWatchpointValue(lineNumber: Int, name: String, value: Any): Unit = {
  }

  override def atWatchpointAssignment(lineNumber: Int, name: String, value: Any): Unit = {
  }

  override def callRoutine(lineNumber: Int, routineName: String): Unit = {

  }

  override def returnRoutine(lineNumber: Int, routineName: String): Unit = {

  }

  override def displayValue(valueString: String): Unit = {
    println(valueString)
  }

  override def displayCallStack(stack: util.ArrayList[Any]): Unit = {
    // Call stack header.
    print(DEBUGGER_ROUTINE_TAG + -1)

    for (item <- stack.asScala) {
      // Name of a procedure or function.
      if (item.isInstanceOf[SymTabEntry]) {
        val routineId = item.asInstanceOf[SymTabEntry]
        val routineName = routineId.getName
        val level = routineId.getSymTab.getNestingLevel
        val definition = routineId.getDefinition

        println(DEBUGGER_ROUTINE_TAG + level + ": " + definition.getText.toUpperCase + " " + routineName)
      }

      // Variable name-value pair.
      else if (item.isInstanceOf[NameValuePair]) {
        val pair = item.asInstanceOf[NameValuePair]
        print(DEBUGGER_VARIABLE_TAG + pair.getVariableName + ": ")
        displayValue(pair.getValueString)
      }
    }

    // Call stack footer.
    println(DEBUGGER_ROUTINE_TAG + -2)
  }

  override def quit(): Unit = {
    println(INTERPRETER_TAG + "Program terminated.")
    System.exit(-1)
  }

  override def commandError(errorMessage: String): Unit = {
    runtimeError(errorMessage, 0)
  }

  override def runtimeError(errorMessage: String, lineNumber: Integer): Unit = {
    print("!!! RUNTIME ERROR")
    if (lineNumber != null) {
      print(" AT LINE " + String.format("%03d", lineNumber))
    }
    println(": " + errorMessage)
  }
}
