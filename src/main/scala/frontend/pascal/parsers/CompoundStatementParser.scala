package frontend.pascal.parsers

import frontend.Token
import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import intermediate.icodeimpl.ICodeNodeTypeImpl
import intermediate.{ICodeFactory, ICodeNode}

// TODO:
//  should it really extend the StatementParser class???

/**
 * The parser of compound statements.
 * @param pascalParser parent parser
 */
class CompoundStatementParser(pascalParser: PascalParserTD) extends StatementParser(pascalParser) {
  override def parse(toket: Token): ICodeNode = {
    var curToken = nextToken() // consume the BEGIN

    // Create the COMPOUND node.
    var compoundNode: ICodeNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.COMPOUND)

    // Parse the statement list terminated by the END token.
    val statementParser = new StatementParser(this)
    statementParser.parseList(curToken, compoundNode, PascalTokenType.END, PascalErrorCode.MISSING_END)

    compoundNode
  }
}
