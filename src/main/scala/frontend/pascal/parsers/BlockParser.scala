package frontend.pascal.parsers

import frontend.Token
import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import intermediate.icodeimpl.ICodeNodeTypeImpl
import intermediate.{ICodeFactory, ICodeNode, SymTabEntry}

/**
  * Parses Pascal blocks (const, type, var declarations and then a compound statement).
  *
  * @param parent parent parser to start where the last parser left off.
  */
class BlockParser(parent: PascalParserTD) extends PascalParserTD(parent) {

  /**
    * Parse method.
    *
    * @param toket     start token.
    * @param routineId id of the routine.
    * @return node of the parse tree.
    */
  def parse(toket: Token, routineId: SymTabEntry): ICodeNode = {
    val declarationsParser = new DeclarationsParser(this)
    val statementParser = new StatementParser(this)

    // Parse any declaration.
    declarationsParser.parse(toket, routineId) // fills up this routine's symbol table with const, type and var definitions.

    val curToken = synchronize(StatementParser.STMT_START_SET) // skip junk.
    val tokenType = curToken.getTokenType
    var rootNode: ICodeNode = null

    // Look for the BEGIN token to parse a compound statement.
    if (tokenType == PascalTokenType.BEGIN) {
      rootNode = statementParser.parse(curToken)
    }
    // Missing BEGIN: Attempt to parse anyway if possible.
    else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_BEGIN, this)

      if (StatementParser.STMT_START_SET.contains(tokenType)) {
        rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.COMPOUND)
        statementParser.parseList(curToken, rootNode, PascalTokenType.END, PascalErrorCode.MISSING_END)
      }
    }

    rootNode
  }
}
