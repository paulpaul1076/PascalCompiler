package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{Parser, Token}
import intermediate.symtabimpl.DefinitionImpl
import intermediate.typeimpl.{TypeFormImpl, TypeKeyImpl}
import intermediate.{TypeFactory, TypeSpec}

class RecordTypeParser(parent: PascalParserTD) extends PascalParserTD(parent) {
  def parse(token: Token): TypeSpec = {
    val recordType = TypeFactory.createType(TypeFormImpl.RECORD)
    var curToken = nextToken() // consume RECORD

    // Push a symbol table for the RECORD type specification.
    recordType.setAttribute(TypeKeyImpl.RECORD_SYMTAB, Parser.symTabStack.push())

    // Parse the field declarations.
    val variableDeclarationsParser = new VariableDeclarationsParser(this)
    variableDeclarationsParser.setDefinition(DefinitionImpl.FIELD)
    variableDeclarationsParser.parse(curToken)

    // Pop off the record's symbol table.
    Parser.symTabStack.pop()

    // Synchronize at the END.
    curToken = synchronize(RecordTypeParser.END_SET)

    // Look for the END.
    if (curToken.getTokenType == PascalTokenType.END) {
      curToken = nextToken() // consume END
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_END, this)
    }

    recordType
  }
}

object RecordTypeParser {
  val END_SET = DeclarationsParser.VAR_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  END_SET.add(PascalTokenType.END)
  END_SET.add(PascalTokenType.SEMICOLON)
}
