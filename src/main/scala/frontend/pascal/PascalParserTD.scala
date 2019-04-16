package frontend.pascal

import java.io.IOException
import java.util

import frontend.pascal.parsers.BlockParser
import frontend.{EofToken, Parser, Scanner, Token}
import intermediate.symtabimpl.{DefinitionImpl, Predefined, SymTabKeyImpl}
import intermediate.{ICodeFactory, SymTabEntry}
import message.{Message, MessageType}

/**
  * Top down pascal parser.
  *
  * @param scanner lexer for this parser.
  */
class PascalParserTD(scanner: Scanner) extends Parser(scanner) {

  /**
    * Name of the routine being parsed.
    */
  private var routineId: SymTabEntry = _

  def this(pascalParser: PascalParserTD) {
    this(pascalParser.getScanner)
  }

  /**
    * Fetches the next token by means of scanner.
    * For each token it sends a token message to every subscribed listener.
    */
  override def parse(): Unit = {
    val startTime = System.currentTimeMillis()
    val iCode = ICodeFactory.createICode()
    Predefined.initialize(Parser.symTabStack)

    // Create a dummy program identifier symbol table entry.
    routineId = Parser.symTabStack.enterLocal("DummyProgramName".toLowerCase)
    routineId.setDefinition(DefinitionImpl.PROGRAM)
    Parser.symTabStack.setProgramId(routineId)

    // Push a new symbol table onto the symbol table stack and set
    // the routine's symbol table and intermediate code.
    routineId.setAttribute(SymTabKeyImpl.ROUTINE_SYMTAB, Parser.symTabStack.push())
    routineId.setAttribute(SymTabKeyImpl.ROUTINE_ICODE, iCode)

    val blockParser = new BlockParser(this)

    try {
      var token = nextToken()

      // Parse a block
      val rootNode = blockParser.parse(token, routineId)
      iCode.setRoot(rootNode)
      Parser.symTabStack.pop()

      // Look for the final period.
      token = currentToken()
      if (token.getTokenType != PascalTokenType.DOT) {
        PascalParserTD.errorHandler.flag(token, PascalErrorCode.MISSING_PERIOD, this)
      }

      token = currentToken()

      // Send the parser summary message.
      val elapsedTime = (System.currentTimeMillis() - startTime) / 1000f
      sendMessage(new Message(MessageType.PARSER_SUMMARY, List[Number](token.getLineNumber, getErrorCount, elapsedTime)))

    } catch {
      case _: IOException => PascalParserTD.errorHandler.abortTranslation(PascalErrorCode.IO_ERROR, this)
    }
  }

  /**
    * Return the number of syntax errors found by the parser.
    *
    * @return error count.
    */
  override def getErrorCount: Int = {
    PascalParserTD.errorHandler.getErrorCount
  }

  /**
    * Synchronize the parser.
    *
    * @param syncSet the set of token types for synchronizing the parser.
    * @return the token where the parser has synchronized.
    */
  def synchronize(syncSet: util.HashSet[PascalTokenType]): Token = {
    var token = currentToken()

    // If the current token is not in the synchronization set,
    // then it is unexpected and the parser must recover.
    if (!syncSet.contains(token.getTokenType)) {
      // Flag the unexpected token.
      PascalParserTD.errorHandler.flag(token, PascalErrorCode.UNEXPECTED_TOKEN, this)

      // Recover by skipping tokens that are not
      // in the synchronization set.
      do {
        token = nextToken()
      } while (!token.isInstanceOf[EofToken] && !syncSet.contains(token.getTokenType))
    }
    token
  }

  protected object PascalParserTD {
    val errorHandler = new PascalErrorHandler
  }

}
