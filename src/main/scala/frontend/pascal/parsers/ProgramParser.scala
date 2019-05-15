package frontend.pascal.parsers

import java.util

import frontend.Token
import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import intermediate.SymTabEntry

// TODO: this parser isn't specific to pascal, so it might have to move some place else
class ProgramParser(parent: PascalParserTD) extends DeclarationsParser(parent) {
  override def parse(toket: Token, parentId: SymTabEntry): SymTabEntry = {
    var curToken = synchronize(ProgramParser.PROGRAM_START_SET)
    // Parse the program
    val routineParser = new DeclaredRoutineParser(this)
    routineParser.parse(curToken, parentId)

    // Look for the final period.
    curToken = currentToken()
    if (curToken.getTokenType != PascalTokenType.DOT) {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_PERIOD, this)
    }
    null
  }
}

object ProgramParser {
  val PROGRAM_START_SET = new util.HashSet[PascalTokenType]()
  PROGRAM_START_SET.add(PascalTokenType.PROGRAM)
  PROGRAM_START_SET.add(PascalTokenType.SEMICOLON)
  PROGRAM_START_SET.addAll(DeclarationsParser.DECLARATION_START_SET)
}
