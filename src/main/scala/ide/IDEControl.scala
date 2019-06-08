package ide

import java.awt.Color

/**
  * The master interface of the Pascal ide.
  */
trait IDEControl {

  def setSourcePath(sourcePath: String): Unit

  def getSourcePath: String

  def setInputPath(inputPath: String): Unit

  def getInputPath: String

  def startDebuggerProcess(sourceName: String): Unit

  def stopDebuggerProcess(): Unit

  def sendToDebuggerProcess(text: String): Unit

  def setEditWindowMessage(message: String, color: Color): Unit

  def clearEditWindowErrors(): Unit

  def addToEditWindowErrors(line: String): Unit

  def showDebugWindow(sourceName: String): Unit

  def clearDebugWindowListing(): Unit

  def addToDebugWindowListing(line: String): Unit

  def selectDebugWindowListingLine(lineNumber: Int): Unit

  def setDebugWindowAtListingLine(lineNumber: Int): Unit

  def setDebugWindowMessage(message: String, color: Color): Unit

  def stopDebugWindow(): Unit

  def showCallStackWindow(sourceName: String)

  def initializeCallStackWindow(): Unit

  def addRoutineToCallStackWindow(level: String, header: String): Unit

  def addVariableToCallStackWindow(name: String, value: String): Unit

  def completeCallStackWindow(): Unit

  def showConsoleWindow(sourceName: String): Unit

  def clearConsoleWindowOutput(): Unit

  def addToConsoleWindowOutput(text: String): Unit

  def enableConsoleWindowInput(): Unit

  def disableConsoleWindowInput(): Unit

  def breakDebugWindowAtListingLine(lineNumber: Int): Unit

}

object IDEControl {
  val LISTING_TAG = "!LISTING:"
  val PARSER_TAG = "!PARSER:"
  val SYNTAX_TAG = "!SYNTAX:"
  val INTERPRETER_TAG = "!INTERPRETER:"

  val DEBUGGER_AT_TAG = "!DEBUGGER.AT:"
  val DEBUGGER_BREAK_TAG = "!DEBUGGER.BREAK:"
  val DEBUGGER_ROUTINE_TAG = "!DEBUGGER.ROUTINE:"
  val DEBUGGER_VARIABLE_TAG = "!DEBUGGER.VARIABLE:"
}
