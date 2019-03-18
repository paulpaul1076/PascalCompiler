package pascal

import java.io.{BufferedReader, FileReader}

import backend.{Backend, BackendFactory}
import frontend.{FrontendFactory, Parser, Source}
import intermediate.{ICode, SymTab, SymTabStack}
import pascal.listeners.{BackendMessageListener, ParserMessageListener, SourceMessageListener}
import util.CrossReferencer

/**
 * Compile or interpret a Pascal source program.
 */
class Pascal(operation: String, filePath: String, flags: String) {

  private var parser: Parser = _
  private var source: Source = _
  private var iCode: ICode = _
  //private var symTab: SymTab = _ // TODO: Remove it altogether?
  private var backend: Backend = _

  private var symTabStack: SymTabStack = _

  try {
    val intermediate = flags.indexOf('i') > -1
    val xref = flags.indexOf('x') > -1

    source = new Source(new BufferedReader(new FileReader(filePath)))
    source.addMessageListener(new SourceMessageListener())

    parser = FrontendFactory.createParser("Pascal", "top-down", source)
    parser.addMessageListener(new ParserMessageListener())

    backend = BackendFactory.createBackend(operation)
    backend.addMessageListener(new BackendMessageListener())

    parser.parse()
    source.close()

    iCode = parser.getICode
    //symTab = parser.getSymTab // TODO: Remove it altogether?
    symTabStack = Parser.symTabStack

    if (xref) {
      val crossReferencer = new CrossReferencer
      crossReferencer.print(symTabStack)
    }

    backend.process(iCode, symTabStack)
  } catch {
    case e: Exception => println("*****Internal translator error. *****")
      e.printStackTrace()
  }
}

/**
 * Companion object.
 */
object Pascal {
  val FLAGS = "[-ix]"
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

      if (i < args.length) {
        val path = args(i)
        new Pascal(operation, path, flags)
      } else {
        throw new Exception
      }
    } catch {
      case _: Exception => println(Pascal.USAGE)
    }
  }
}
