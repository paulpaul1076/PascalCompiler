package frontend.pascal.parsers

import java.util

import frontend.Token
import frontend.pascal.{PascalParserTD, PascalTokenType}
import intermediate.SymTabEntry
import intermediate.symtabimpl.DefinitionImpl

/**
  * Parses declarations.
  *
  * @param parent parent parser needed to pick up where we left off.
  */
class DeclarationsParser(parent: PascalParserTD) extends PascalParserTD(parent) {

  /**
    * The parse method.
    *
    * @param toket starting token.
    */
  def parse(toket: Token, parentId: SymTabEntry): SymTabEntry = {
    var curToken = synchronize(DeclarationsParser.DECLARATION_START_SET)

    if (curToken.getTokenType == PascalTokenType.CONST) {
      curToken = nextToken() // consume CONST

      val constantDefinitionsParser = new ConstantDefinitionsParser(this)
      constantDefinitionsParser.parse(curToken) // TODO: pass null
    }

    curToken = synchronize(DeclarationsParser.TYPE_START_SET)

    if (curToken.getTokenType == PascalTokenType.TYPE) {
      curToken = nextToken() // consume TYPE

      val typeDefinitionsParser = new TypeDefinitionsParser(this)
      typeDefinitionsParser.parse(curToken) // TODO: pass null
    }

    curToken = synchronize(DeclarationsParser.VAR_START_SET)

    if (curToken.getTokenType == PascalTokenType.VAR) {
      curToken = nextToken() // consume VAR

      val variableDeclarationsParser = new VariableDeclarationsParser(this)
      variableDeclarationsParser.setDefinition(DefinitionImpl.VARIABLE) // TODO: why is this necessary? Answer: for variable declaration parser!
      variableDeclarationsParser.parse(curToken) // TODO: pass null?????
    }

    curToken = synchronize(DeclarationsParser.ROUTINE_START_SET)
    var tokenType = curToken.getTokenType

    while (tokenType == PascalTokenType.PROCEDURE || tokenType == PascalTokenType.FUNCTION) {
      val routineParser = new DeclaredRoutineParser(this)
      routineParser.parse(curToken, parentId)

      // Look for one or more semicolons after a definition.
      curToken = currentToken()
      if (curToken.getTokenType == PascalTokenType.SEMICOLON) {
        while (curToken.getTokenType == PascalTokenType.SEMICOLON) {
          curToken = nextToken() // consume the ;
        }
      } // TODO: find out if the semicolons are optional

      curToken = synchronize(DeclarationsParser.ROUTINE_START_SET) // TODO: why don't we synchronize at the beginning of the loop here?
      tokenType = curToken.getTokenType
    }
    null
  }
}

/**
  * Companion object.
  */
object DeclarationsParser {
  /**
    * This is how a declaration can start.
    */
  val DECLARATION_START_SET = new util.HashSet[PascalTokenType]
  DECLARATION_START_SET.add(PascalTokenType.CONST)
  DECLARATION_START_SET.add(PascalTokenType.TYPE)
  DECLARATION_START_SET.add(PascalTokenType.VAR)
  DECLARATION_START_SET.add(PascalTokenType.PROCEDURE)
  DECLARATION_START_SET.add(PascalTokenType.FUNCTION)
  DECLARATION_START_SET.add(PascalTokenType.BEGIN)

  /**
    * Below are constants for the sections that can be found in a declarations block: type, var
    * (const is the same as DECLARATION_START_SET that's why it's missing).
    */
  val TYPE_START_SET = DECLARATION_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  TYPE_START_SET.remove(PascalTokenType.CONST)

  val VAR_START_SET = TYPE_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  VAR_START_SET.remove(PascalTokenType.TYPE)

  /**
    * In case there's no declaration, I guess????
    */
  val ROUTINE_START_SET = VAR_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  ROUTINE_START_SET.remove(PascalTokenType.VAR)
}