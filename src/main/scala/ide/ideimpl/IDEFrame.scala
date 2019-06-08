package ide.ideimpl

import ide._
import ide.ideimpl._
import java.awt._
import java.awt.event._
import javax.swing._


/**
  * <h1>IDEFrame</h1>
  *
  * <p>The main window of the Pascal IDE.</p>
  *
  * <p>Copyright (c) 2009 by Ronald Mak</p>
  * <p>For instructional purposes only.  No warranties.</p>
  */
object IDEFrame {
  private val TITLE = "Pascal IDE"
  private val WINDOW_SPACING = 10
  private val HEADER_HEIGHT = 32
  private val BORDER_SIZE = 5
  private val EDITOR_WIDTH = 600
  private val EDITOR_HEIGHT = 600
  private val STACK_WIDTH = 400
  private val STACK_HEIGHT = 600
  private val CONSOLE_WIDTH = EDITOR_WIDTH + WINDOW_SPACING + STACK_WIDTH
  private val CONSOLE_HEIGHT = 350
  private val DEBUGGER_WIDTH = 600
  private val DEBUGGER_HEIGHT = EDITOR_HEIGHT + WINDOW_SPACING + CONSOLE_HEIGHT
  private val IDE_WIDTH = EDITOR_WIDTH + WINDOW_SPACING + STACK_WIDTH + WINDOW_SPACING + DEBUGGER_WIDTH + 2 * BORDER_SIZE
  private val IDE_HEIGHT = HEADER_HEIGHT + DEBUGGER_HEIGHT + 2 * BORDER_SIZE
}

class IDEFrame extends JFrame with IDEControl {

  val screenSize: Dimension = Toolkit.getDefaultToolkit.getScreenSize
  val frameSize = new Dimension(IDEFrame.IDE_WIDTH, IDEFrame.IDE_HEIGHT)
  private val desktop = new JDesktopPane
  private var editFrame: EditFrame = _
  private var debugFrame = new DebugFrame(this)
  private var consoleFrame: ConsoleFrame = _
  private var stackFrame: CallStackFrame = _
  private var debuggerProcess: DebuggerProcess = _
  private var sourcePath: String = _
  private var inputPath: String = _

  setSize(frameSize)
  setLocation((screenSize.width - frameSize.width) / 2, 10)
  setTitle(IDEFrame.TITLE)
  setVisible(true)
  validate()
  val contentPanel: JPanel = this.getContentPane.asInstanceOf[JPanel]
  contentPanel.add(desktop)
  desktop.setBackground(new Color(215, 215, 255))
  // Editor window.
  editFrame = new EditFrame(this)
  editFrame.setSize(IDEFrame.EDITOR_WIDTH, IDEFrame.EDITOR_HEIGHT)
  editFrame.setLocation(0, 0)
  desktop.add(editFrame)
  editFrame.setVisible(true)
  // Debugger window.

  debugFrame.setSize(IDEFrame.DEBUGGER_WIDTH, IDEFrame.DEBUGGER_HEIGHT)
  debugFrame.setLocation(IDEFrame.IDE_WIDTH - IDEFrame.BORDER_SIZE - IDEFrame.DEBUGGER_WIDTH, 0)
  desktop.add(debugFrame)
  debugFrame.setVisible(true)
  // Console window.
  consoleFrame = new ConsoleFrame(this)
  consoleFrame.setSize(IDEFrame.CONSOLE_WIDTH, IDEFrame.CONSOLE_HEIGHT)
  consoleFrame.setLocation(0, IDEFrame.EDITOR_HEIGHT + IDEFrame.WINDOW_SPACING)
  desktop.add(consoleFrame)
  consoleFrame.setVisible(true)
  // Call stack window.
  stackFrame = new CallStackFrame(this)
  stackFrame.setSize(IDEFrame.STACK_WIDTH, IDEFrame.STACK_HEIGHT)
  stackFrame.setLocation(IDEFrame.EDITOR_WIDTH + IDEFrame.WINDOW_SPACING, 0)
  desktop.add(stackFrame)
  stackFrame.setVisible(true)

  /**
    * Process a window event.
    *
    * @param event the event.
    */
  override protected def processWindowEvent(event: WindowEvent): Unit = {
    super.processWindowEvent(event)
    if (event.getID == WindowEvent.WINDOW_CLOSING) {
      if (debuggerProcess != null) debuggerProcess.kill()
      debugFrame.stop()
      System.exit(0)
    }
  }

  /**
    * Set the path of the source file.
    *
    * @param sourcePath the path.
    */
  def setSourcePath(sourcePath: String): Unit = {
    this.sourcePath = sourcePath
  }

  /**
    * @return the path of the source file.
    */
  def getSourcePath: String = if (sourcePath != null) sourcePath
  else ""

  /**
    * Set the path of the runtime input data file.
    *
    * @param inputPath the path.
    */
  def setInputPath(inputPath: String): Unit = {
    this.inputPath = inputPath
  }

  /**
    * @return the path of the runtime input data file.
    */
  def getInputPath: String = if (inputPath != null) inputPath
  else ""

  /**
    * Start the debugger process.
    *
    * @param sourceName the source file name.
    */
  def startDebuggerProcess(sourceName: String): Unit = {
    debuggerProcess = new DebuggerProcess(this, sourceName)
    debuggerProcess.start()
  }

  /**
    * Stop the debugger process.
    */
  def stopDebuggerProcess(): Unit = {
    if (debuggerProcess != null) {
      debuggerProcess.kill()
      debuggerProcess = null
    }
  }

  /**
    * Send a command or runtime input text to the debugger process.
    *
    * @param text the command string or input text.
    */
  def sendToDebuggerProcess(text: String): Unit = {
    debuggerProcess.writeToDebuggerStandardInput(text)
  }

  /**
    * Set the editor window's message.
    *
    * @param message the message.
    * @param color   the message color.
    */
  def setEditWindowMessage(message: String, color: Color): Unit = {
    editFrame.setMessage(message, color)
  }

  /**
    * Clear the editor window's syntax errors.
    */
  def clearEditWindowErrors(): Unit = {
    editFrame.clearEditWindowErrors()
  }

  /**
    * Add a syntax error message to the editor window's syntax errors.
    *
    * @param line the error message.
    */
  def addToEditWindowErrors(line: String): Unit = {
    editFrame.addError(line)
  }

  /**
    * Show the debugger window.
    *
    * @param sourceName the source file name.
    */
  def showDebugWindow(sourceName: String): Unit = {
    desktop.getDesktopManager.deiconifyFrame(debugFrame)
    debugFrame.setTitle("DEBUG: " + sourceName)
    debugFrame.setVisible(true)
    debugFrame.initialize()
  }

  /**
    * Clear the debugger window's listing.
    */
  def clearDebugWindowListing(): Unit = {
    debugFrame.clearListing()
  }

  /**
    * Add a line to the debugger window's listing.
    *
    * @param line the listing line.
    */
  def addToDebugWindowListing(line: String): Unit = {
    debugFrame.addListingLine(line)
  }

  /**
    * Select a listing line in the debugger window.
    *
    * @param lineNumber the line number.
    */
  def selectDebugWindowListingLine(lineNumber: Int): Unit = {
    debugFrame.selectListingLine(lineNumber)
  }

  /**
    * Set the debugger to a listing line.
    *
    * @param lineNumber the line number.
    */
  def setDebugWindowAtListingLine(lineNumber: Int): Unit = {
    debugFrame.atListingLine(lineNumber)
  }

  /**
    * Set the debugger to break at a listing line.
    *
    * @param lineNumber the line number.
    */
  def breakDebugWindowAtListingLine(lineNumber: Int): Unit = {
    debugFrame.breakAtListingLine(lineNumber)
  }

  /**
    * Set the debugger window's message.
    *
    * @param message the message.
    * @param color   the message color.
    */
  def setDebugWindowMessage(message: String, color: Color): Unit = {
    debugFrame.setMessage(message, color)
  }

  /**
    * Stop the debugger.
    */
  def stopDebugWindow(): Unit = {
    debugFrame.stop()
  }

  /**
    * Show the call stack window.
    *
    * @param sourceName the source file name.
    */
  def showCallStackWindow(sourceName: String): Unit = {
    desktop.getDesktopManager.deiconifyFrame(stackFrame)
    stackFrame.setTitle("CALL STACK: " + sourceName)
    stackFrame.setVisible(true)
    stackFrame.initialize()
    stackFrame.toBack()
    try
      debugFrame.setSelected(true)
    catch {
      case ignore: Exception =>

    }
  }

  /**
    * Initialize the call stack display.
    */
  def initializeCallStackWindow(): Unit = {
    stackFrame.initialize()
  }

  /**
    * Add an invoked routine to the call stack display.
    *
    * @param level  the routine's nesting level.
    * @param header the routine's header.
    */
  def addRoutineToCallStackWindow(level: String, header: String): Unit = {
    stackFrame.addRoutine(level, header)
  }

  /**
    * Add a local variable to the call stack display.
    *
    * @param name  the variable's name.
    * @param value the variable's value.
    */
  def addVariableToCallStackWindow(name: String, value: String): Unit = {
    stackFrame.addVariable(name, value)
  }

  /**
    * Complete the call stack display.
    */
  def completeCallStackWindow(): Unit = {
    stackFrame.complete()
  }

  /**
    * Show the console window.
    *
    * @param sourceName the source file name.
    */
  def showConsoleWindow(sourceName: String): Unit = {
    desktop.getDesktopManager.deiconifyFrame(consoleFrame)
    consoleFrame.setTitle("CONSOLE: " + sourceName)
    consoleFrame.setVisible(true)
    consoleFrame.initialize()
    consoleFrame.toBack()
    try
      debugFrame.setSelected(true)
    catch {
      case ignore: Exception =>

    }
  }

  /**
    * Clear the console window's output.
    */
  def clearConsoleWindowOutput(): Unit = {
    consoleFrame.clearOutput()
  }

  /**
    * Add output text to the console window.
    *
    * @param text the output text.
    */
  def addToConsoleWindowOutput(text: String): Unit = {
    consoleFrame.addToOutput(text)
  }

  /**
    * Enable runtime input from the console window.
    */
  def enableConsoleWindowInput(): Unit = {
    consoleFrame.enableInput()
  }

  /**
    * Disable runtime input from the console window.
    */
  def disableConsoleWindowInput(): Unit = {
    consoleFrame.disableInput()
  }
}
