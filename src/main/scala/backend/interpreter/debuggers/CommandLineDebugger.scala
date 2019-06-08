package backend.interpreter.debuggers

import java.util

import backend.Backend
import backend.interpreter.{Debugger, RuntimeStack}
import intermediate.SymTabEntry
import message.Message

import scala.collection.JavaConverters._

/**
  * Command line debugger implementation.
  *
  * @param backend      backend.
  * @param runtimeStack runtime stack.
  */
class CommandLineDebugger(backend: Backend, runtimeStack: RuntimeStack) extends Debugger(backend, runtimeStack) {
  private val commandProcessor = new CommandProcessor(this)

  override def processMessage(message: Message): Unit = {
    commandProcessor.processMessage(message)
  }

  override def promptForCommand(): Unit = {
    print(">>> Command? ")
  }

  override def parseCommand(): Boolean = {
    return commandProcessor.parseCommand()
  }

  override def atStatement(lineNumber: Int): Unit = {
    println("\n>>> At line " + lineNumber)
  }

  override def atBreakpoint(lineNumber: Int): Unit = {
    println("\n>>> Breakpoint at line " + lineNumber)
  }

  override def atWatchpointValue(lineNumber: Int, name: String, value: Any): Unit = {
    println("\n>>> At line " + lineNumber + ": " + name + " : " + value.toString)
  }

  override def atWatchpointAssignment(lineNumber: Int, name: String, value: Any): Unit = {
    println("\n>>> At line " + lineNumber + ": " + name + " := " + value.toString)
  }

  override def callRoutine(lineNumber: Int, routineName: String): Unit = {

  }

  override def returnRoutine(lineNumber: Int, routineName: String): Unit = {

  }

  override def displayValue(valueString: String): Unit = {
    println(valueString)
  }

  override def displayCallStack(stack: util.ArrayList[Any]): Unit = {
    for (item <- stack.asScala) {
      // Name of a procedure or function.
      if (item.isInstanceOf[SymTabEntry]) {
        val routineId = item.asInstanceOf[SymTabEntry]
        val routineName = routineId.getName
        val level = routineId.getSymTab.getNestingLevel
        val definition = routineId.getDefinition

        println(level + ": " + definition.getText.toUpperCase + " " + routineName)
      }

      // Variable name-value pair.
      else if (item.isInstanceOf[NameValuePair]) {
        val pair = item.asInstanceOf[NameValuePair]
        print("  " + pair.getVariableName + ": ")
        displayValue(pair.getValueString)
      }
    }
  }

  override def quit(): Unit = {
    println("Program terminated.")
    System.exit(-1)
  }

  override def commandError(errorMessage: String): Unit = {
    println("!!! ERROR: " + errorMessage)
  }

  override def runtimeError(errorMessage: String, lineNumber: Integer): Unit = {
    print("!!! RUNTIME ERROR")
    if (lineNumber != null) {
      print(" at line " + String.format("%03d", lineNumber))
    }
    println(": " + errorMessage)
  }
}
