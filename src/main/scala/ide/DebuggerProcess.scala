package ide

import java.awt._
import java.io._

import ide.IDEControl._


/**
  * <h1>DebuggerProcess</h1>
  *
  * <p>The debugger process of the Pascal IDE.</p>
  *
  * <p>Copyright (c) 2009 by Ronald Mak</p>
  * <p>For instructional purposes only.  No warranties.</p>
  */
object DebuggerProcess { // The command that starts the debugger process.
  private val COMMAND = "java -classpath classes Pascal execute %s %s"
}

class DebuggerProcess(var control: IDEControl // the IDE control interface
                      , var sourceName: String // source file name
                     ) extends Thread {

  private var process: Process = _ // the debugger process

  private var toDebuggerStream: PrintWriter = _ // IDE to debugger I/O stream

  private var debuggerOutput: DebuggerOutput = _ // debugger process output

  private var haveSyntaxErrors = false // true if have syntax errors

  private val debugging = false // true if debugging process I/O

  /**
    * Run the procecess.
    */
  override def run(): Unit = {
    try { // Start the Pascal debugger process.
      val command = String.format(DebuggerProcess.COMMAND, control.getSourcePath, control.getInputPath)
      process = Runtime.getRuntime.exec(command)
      // Capture the process's input stream.
      toDebuggerStream = new PrintWriter(process.getOutputStream)
      //process.getErrorStream
      // Read and dispatch output text from the
      // debugger process for processing.
      debuggerOutput = new DebuggerOutput(process)
      dispatchDebuggerOutput()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }
  }

  /**
    * Kill the debugger process.
    */
  def kill(): Unit = {
    if (process != null) {
      process.destroy()
      process = null
    }
  }

  /**
    * Write a command or runtime input text
    * to the debugger process's standard input.
    *
    * @param text the command string or input text.
    */
  def writeToDebuggerStandardInput(text: String): Unit = {
    classOf[DebuggerProcess] synchronized toDebuggerStream.println(text)
    toDebuggerStream.flush()
    if (debugging) System.err.println("Sent: '" + text + "'")

  }

  /**
    * Read and dispatch output text from the debugger process for processing.
    *
    * @throws Exception if an error occurred.
    */
  @throws[Exception]
  private def dispatchDebuggerOutput(): Unit = {
    var text: String = null
    // Loop to process debugger output text
    // which may contain embedded output tags.
    do {
      text = debuggerOutput.next
      if (debugging) System.out.println("Read: '" + text + "'")
      var index = 0
      do {
        Thread.sleep(1)
        index = text.indexOf('!')
        // The debugger output text contains the ! character.
        // It may be the start of an output tag.
        if (index >= 0) { // Add any preceding text to the console window.
          if (index > 0) {
            val precedingText = text.substring(0, index)
            control.addToConsoleWindowOutput(precedingText)
            text = text.substring(index)
          }
          // Yes, it was an output tag. Don't loop again.
          if (processTag(text)) index = -1
          else { // No, it wasn't.
            // Loop again to process the rest of the output text.
            control.addToConsoleWindowOutput("!")
            text = text.substring(1)
          }
        }
        else { // Send all the debugger output text to the console window.
          control.addToConsoleWindowOutput(text)
        }
      } while ( {
        index >= 0
      })
    } while ( {
      !text.startsWith(INTERPRETER_TAG)
    })
  }

  /**
    * Process a tag in the output text.
    *
    * @param text1 the output text
    * @return true if processed a tag, else false.
    */
  private def processTag(text1: String) = { // Listing line.
    var text = text1
    if (text.startsWith(LISTING_TAG)) {
      control.addToDebugWindowListing(text.substring(LISTING_TAG.length))
      true
    }
    else { // Syntax error message.
      if (text.startsWith(SYNTAX_TAG)) {
        val errorMessage = text.substring(SYNTAX_TAG.length)
        control.addToEditWindowErrors(errorMessage)
        haveSyntaxErrors = true
        true
      }
      else { // Parser message.
        if (text.startsWith(PARSER_TAG)) {
          control.setEditWindowMessage(text.substring(PARSER_TAG.length), if (haveSyntaxErrors) Color.RED
          else Color.BLUE)
          if (!haveSyntaxErrors) {
            control.clearEditWindowErrors
            control.setDebugWindowMessage("", Color.BLUE)
            control.showDebugWindow(sourceName)
            control.showCallStackWindow(sourceName)
            control.showConsoleWindow(sourceName)
          }
          else {
            control.setDebugWindowMessage("Fix syntax errors.", Color.RED)
            control.stopDebugWindow
            control.disableConsoleWindowInput
          }
          true
        }
        else { // Debugger at a source statement.
          if (text.startsWith(DEBUGGER_AT_TAG)) {
            val lineNumber = text.substring(DEBUGGER_AT_TAG.length)
            control.setDebugWindowAtListingLine(lineNumber.trim.toInt)
            control.setDebugWindowMessage(" ", Color.BLUE)
            true
          }
          else { // Debugger break at a source statement.
            if (text.startsWith(DEBUGGER_BREAK_TAG)) {
              val lineNumber = text.substring(DEBUGGER_BREAK_TAG.length)
              control.breakDebugWindowAtListingLine(lineNumber.trim.toInt)
              control.setDebugWindowMessage("Break at text " + lineNumber, Color.BLUE)
              true
            }
            else { // Debugger add a routine to the call stack.
              if (text.startsWith(DEBUGGER_ROUTINE_TAG)) {
                val components = text.split(":")
                val level = components(1).trim
                // Header.
                if (level == "-1") control.initializeCallStackWindow
                else { // Footer.
                  if (level == "-2") control.completeCallStackWindow
                  else { // Routine name.
                    val header = components(2).trim
                    control.addRoutineToCallStackWindow(level, header)
                  }
                }
                true
              }
              else { // Debugger add a local variable to the call stack.
                if (text.startsWith(DEBUGGER_VARIABLE_TAG)) {
                  text = text.substring(DEBUGGER_VARIABLE_TAG.length)
                  val index = text.indexOf(":")
                  val name = text.substring(0, index)
                  val value = text.substring(index + 1)
                  control.addVariableToCallStackWindow(name, value)
                  true
                }
                else { // Interpreter message.
                  if (text.startsWith(INTERPRETER_TAG)) {
                    control.setDebugWindowMessage(text.substring(INTERPRETER_TAG.length), Color.BLUE)
                    control.stopDebugWindow
                    control.disableConsoleWindowInput
                    true
                  }
                  else false // it wasn't an output tag
                }
              }
            }
          }
        }
      }
    }
  }

  /**
    * Output from the debugger.
    */
  object DebuggerOutput {
    val BUFFER_SIZE = 1024
  }

  class DebuggerOutput(val process: Process) {


     var fromDebuggerStream: BufferedReader = new BufferedReader(new InputStreamReader(process.getInputStream)) // debugger to IDE I/O stream

     var buffer: Array[Char] = new Array[Char](DebuggerOutput.BUFFER_SIZE) // output buffer

     var start = 0 // start of output line

     var index = 0 // index of \n or end of line

     var prevIndex = 0 // previous index

     var length = 0 // output text length

    /**
      * Get the next complete or partial output line.
      *
      * @return the output line.
      * @throws Exception if an error occurred.
      */
    @throws[Exception]
    def next: String = {
      var output = ""
      // Loop to process output from the interpreter.

      while ( {
        true
      }) {
        Thread.sleep(1)
        index = findEol(prevIndex)
        // Found end of line: Return the line.
        if (index < length) {
          output += new String(buffer, start, index - start + 1)
          start = index + 1
          prevIndex = start
          if (debugging) System.err.println("Output: '" + output + "'")
          return output
        }
        // No end of line: Append to the current output.
        if (index > start) output += new String(buffer, start, index - start)
        // Prepare to read again into the buffer.
        start = 0
        length = 0
        prevIndex = 0
        // Read more output if available.
        if (fromDebuggerStream.ready) length = readFromDebuggerStandardOutput
        else { // No more output: Return the current output.
          if (debugging) System.err.println("Output: '" + output + "'")
          return output
        }
      }
      return output
    }

    /**
      * Read debugger status or runtime output
      * from the debugger's standard output.
      *
      * @return the number of characters read.
      * @throws Exception if an error occurred.
      */
    @throws[Exception]
    def readFromDebuggerStandardOutput = fromDebuggerStream.read(buffer)

    /**
      * Look for \n in the output.
      *
      * @param index1 where to start looking.
      * @return the index of \n or the end of output.
      */
    def findEol(index1: Int) = {
      var index = index1
      while ( {
        (index < length) && (buffer(index) != '\n')
      }) index += 1
      index
    }

  }
}
