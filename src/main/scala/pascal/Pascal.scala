package pascal

import java.io.{BufferedReader, File, FileReader}

import backend.{Backend, BackendFactory}
import frontend.{FrontendFactory, Parser, Source}
import intermediate.symtabimpl.SymTabKeyImpl
import intermediate.{ICode, SymTabStack}
import pascal.listeners.{BackendMessageListener, ParserMessageListener, SourceMessageListener}
import util.{CrossReferencer, ParseTreePrinter}

/**
  * Compile or interpret a Pascal source program.
  */
class Pascal(operation: String, sourcePath: String, inputPath: String, flags: String) {

  private var parser: Parser = _
  private var source: Source = _
  private var iCode: ICode = _
  private var backend: Backend = _
  private var symTabStack: SymTabStack = _

  try {
    Pascal.intermediate = flags.indexOf('i') > -1
    Pascal.xref = flags.indexOf('x') > -1
    Pascal.lines = flags.indexOf('l') > -1
    Pascal.assign = flags.indexOf('a) > -1
    Pascal.fetch = flags.indexOf('f') > -1
    Pascal.call = flags.indexOf('c') > -1
    Pascal.`return` = flags.indexOf('r') > -1

    source = new Source(new BufferedReader(new FileReader(sourcePath)))
    source.addMessageListener(new SourceMessageListener())

    parser = FrontendFactory.createParser("Pascal", "top-down", source)
    parser.addMessageListener(new ParserMessageListener())

    backend = BackendFactory.createBackend(operation, inputPath) // execute(interpret) or compile
    backend.addMessageListener(new BackendMessageListener())

    parser.parse()
    source.close()

    if (parser.getErrorCount == 0) {
      symTabStack = Parser.symTabStack
      val programId = symTabStack.getProgramId()
      iCode = programId.getAttribute(SymTabKeyImpl.ROUTINE_ICODE).asInstanceOf[ICode]

      if (Pascal.xref) { // Print the symbol table
        val crossReferencer = new CrossReferencer
        crossReferencer.print(symTabStack)
      }

      if (Pascal.intermediate) { // Print AST
        val treePrinter = new ParseTreePrinter(System.out)
        treePrinter.print(iCode)
      }

      backend.process(iCode, symTabStack)
    }
  } catch {
    case e: Exception => println("*****Internal translator error. *****")
      e.printStackTrace()
  }
}

/**
  * Companion object.
  */
object Pascal {

  var intermediate: Boolean = _
  var xref: Boolean = _
  var lines: Boolean = _
  var assign: Boolean = _
  var fetch: Boolean = _
  var call: Boolean = _
  var `return`: Boolean = _


  val FLAGS = "[-ixlafcr]"
  val USAGE = "Usage: Pascal execute|compile " + FLAGS + " <source file path>"

  val PARSER_SUMMARY_FORMAT = "" +
    "\n%,20d source lines." +
    "\n%,20d syntax errors." +
    "\n%,20.2f seconds total parsing time.\n"
  val INTERPRETER_SUMMARY_FORMAT =
    "\n%,20d statements executed." +
      "\n%,20d runtime errors." +
      "\n%20.2f seconds total execution time.\n"
  val COMPILER_SUMMARY_FORMAT =
    "\n%,20d instruction generated." +
      "\n%,20.2f seconds total code generation time.\n"

  /**
    * Program's entry point.
    *
    * @param args cmd args.
    */
  def main(args: Array[String]): Unit = {
    try {
      val operation = args(0)
      if (!operation.equalsIgnoreCase("compile") &&
        !operation.equalsIgnoreCase("execute")) {
        throw new Exception
      }

      var i = 1
      var flags = "" // compilation flags, starting with '-'

      while (i < args.length && args(i).charAt(0) == '-') {
        flags += args(i).substring(1)
        i += 1
      }

      var sourcePath: String = null
      var inputPath: String = null

      if (i < args.length) {
        sourcePath = args(i)
      } else {
        throw new Exception
      }

      // Runtime input data file path.
      i += 1
      if (i < args.length) {
        inputPath = args(i)

        val inputFile = new File(inputPath)
        if (!inputFile.exists()) {
          println("Input file '" + inputPath + "' does not exist.")
          throw new Exception
        }
      }
      new Pascal(operation, sourcePath, inputPath, flags)
    } catch {
      case _: Exception => println(Pascal.USAGE)
    }
  }
}
