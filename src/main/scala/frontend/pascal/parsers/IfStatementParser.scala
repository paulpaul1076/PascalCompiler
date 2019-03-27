package frontend.pascal.parsers

import frontend.Token
import frontend.pascal.PascalParserTD
import intermediate.ICodeNode

class IfStatementParser(pascalParserTD: PascalParserTD) extends StatementParser(pascalParserTD) {
  override def parse(token: Token): ICodeNode = {
    null
  }
}
