package frontend.pascal.parsers

import frontend.Token
import frontend.pascal.{PascalParserTD, PascalTokenType}
import intermediate.ICodeNode

class WhileStatementParser(pascalParserTD: PascalParserTD) extends StatementParser(pascalParserTD) {
  override def parse(token: Token): ICodeNode = {
    null
  }
}

object WhileStatementParser {
  val DO_SET = StatementParser.STMT_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  DO_SET.add(PascalTokenType.DO)
  DO_SET.addAll(StatementParser.STMT_FOLLOW_SET)
}