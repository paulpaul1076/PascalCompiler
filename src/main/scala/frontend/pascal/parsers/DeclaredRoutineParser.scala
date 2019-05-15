package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{Parser, Token}
import intermediate.symtabimpl.{DefinitionImpl, Predefined, RoutineCodeImpl, SymTabKeyImpl}
import intermediate.typeimpl.TypeFormImpl
import intermediate.{Definition, ICodeFactory, SymTab, SymTabEntry}

/**
  * Parse a main program routine or a declared procedure or function.
  *
  * @param parent parent parser needed to pick up where we left off.
  */
class DeclaredRoutineParser(parent: PascalParserTD) extends DeclarationsParser(parent) {
  override def parse(toket: Token, parentId: SymTabEntry): SymTabEntry = {
    var routineDefn: Definition = null
    var dummyName: String = null
    var routineId: SymTabEntry = null
    var curToken = toket
    var routineType = curToken.getTokenType

    // Initialize.
    routineType.asInstanceOf[PascalTokenType] match {
      case PascalTokenType.PROGRAM =>
        curToken = nextToken() // consume PROGRAM
        routineDefn = DefinitionImpl.PROGRAM
        dummyName = "DummyProgramName".toLowerCase
      case PascalTokenType.PROCEDURE =>
        curToken = nextToken() // consume PROCEDURE
        routineDefn = DefinitionImpl.PROCEDURE
        DeclaredRoutineParser.dummyCounter += 1
        dummyName = "DummyProcedureName_".toLowerCase + f"${DeclaredRoutineParser.dummyCounter}%03d"
      case PascalTokenType.FUNCTION =>
        curToken = nextToken() // consume FUNCTION
        routineDefn = DefinitionImpl.FUNCTION
        DeclaredRoutineParser.dummyCounter += 1
        dummyName = "DummyFunctionName_".toLowerCase + f"${DeclaredRoutineParser.dummyCounter}%03d"
      case _ =>
        routineDefn = DefinitionImpl.PROGRAM
        dummyName = "DummyProgramName".toLowerCase
    }

    // Parse the routine name.
    routineId = parseRoutineName(curToken, dummyName)
    routineId.setDefinition(routineDefn)

    curToken = currentToken()

    // Create new intermediate code for the routine.
    val iCode = ICodeFactory.createICode()
    routineId.setAttribute(SymTabKeyImpl.ROUTINE_ICODE, iCode)
    routineId.setAttribute(SymTabKeyImpl.ROUTINE_ROUTINES, new util.ArrayList[SymTabEntry]())

    // Push the routine's new symbol table onto the stack.
    // If it was forwarded, push its existing symbol table.
    if (routineId.getAttribute(SymTabKeyImpl.ROUTINE_CODE) == RoutineCodeImpl.FORWARD) {
      val symTab = routineId.getAttribute(SymTabKeyImpl.ROUTINE_SYMTAB).asInstanceOf[SymTab]
      Parser.symTabStack.push(symTab)
    } else {
      routineId.setAttribute(SymTabKeyImpl.ROUTINE_SYMTAB, Parser.symTabStack.push())
    }

    // Program: Set the program identifier in the symbol table stack.
    if (routineDefn == DefinitionImpl.PROGRAM) {
      Parser.symTabStack.setProgramId(routineId)
    }
    // Non-forwarded procedure or function: Append to the parent's list
    //                                      of routines.
    else if (routineId.getAttribute(SymTabKeyImpl.ROUTINE_CODE) != RoutineCodeImpl.FORWARD) {
      val subroutines = parentId.getAttribute(SymTabKeyImpl.ROUTINE_ROUTINES).asInstanceOf[util.ArrayList[SymTabEntry]]
      subroutines.add(routineId)
    }

    // If the routine was forwarded, there should not be
    // any formal parameters or a function return type.
    // But parse them anyway if they're there.
    if (routineId.getAttribute(SymTabKeyImpl.ROUTINE_CODE) == RoutineCodeImpl.FORWARD) {
      if (curToken.getTokenType != PascalTokenType.SEMICOLON) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.ALREADY_FORWARDED, this)
        parseHeader(curToken, routineId)
      }
    }
    // Parse the routine's formal parameters and function return type.
    else {
      parseHeader(curToken, routineId)
    }
    // Look for the semicolon.
    curToken = currentToken()
    if (curToken.getTokenType == PascalTokenType.SEMICOLON) {
      do {
        curToken = nextToken() // consume ;
      } while (curToken.getTokenType == PascalTokenType.SEMICOLON)
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_SEMICOLON, this)
    }

    // Parse the routine's block or forward declaration.
    if (curToken.getTokenType == PascalTokenType.IDENTIFIER &&
      curToken.getText.toLowerCase.equalsIgnoreCase("forward")) {
      curToken = nextToken() // consume forward
      routineId.setAttribute(SymTabKeyImpl.ROUTINE_CODE, RoutineCodeImpl.FORWARD)
    } else {
      routineId.setAttribute(SymTabKeyImpl.ROUTINE_CODE, RoutineCodeImpl.DECLARED)

      val blockParser = new BlockParser(this)
      val rootNode = blockParser.parse(curToken, routineId)
      iCode.setRoot(rootNode)
    }

    // Pop the routine's symbol table off the stack.
    Parser.symTabStack.pop()

    routineId
  }

  /**
    * Parse a routine's name.
    *
    * @param toket     the current token.
    * @param dummyName a dummy name in case of a parsing problem.
    * @return the symbol table entry of the declared routine's name.
    */
  def parseRoutineName(toket: Token, dummyName: String): SymTabEntry = {
    var routineId: SymTabEntry = null
    var curToken = toket
    // Parse the routine name identifier.
    if (curToken.getTokenType == PascalTokenType.IDENTIFIER) {
      val routineName = curToken.getText.toLowerCase
      routineId = Parser.symTabStack.lookupLocal(routineName)

      // Not already defined locally: Enter into the local symbol table.
      if (routineId == null) {
        routineId = Parser.symTabStack.enterLocal(routineName)
      }

      // If already defined, it should be a forward definition.
      else if (routineId.getAttribute(SymTabKeyImpl.ROUTINE_CODE) != RoutineCodeImpl.FORWARD) {
        routineId = null
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.IDENTIFIER_REDEFINED, this)
      }

      curToken = nextToken() // consume routine name identifier
    }
    else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_IDENTIFIER, this)
    }

    // If necessary, create a dummy routine name symbol table entry.
    if (routineId == null) {
      routineId = Parser.symTabStack.enterLocal(dummyName)
    }

    routineId
  }

  /**
    * Parse a routine's formal parameter list and the function return type.
    *
    * @param toket     the current token.
    * @param routineId the symbol table entry of the declared routine's name.
    */
  def parseHeader(toket: Token, routineId: SymTabEntry): Unit = {
    var curToken = toket
    // Parse the routine's formal parameters.
    parseFormalParameters(curToken, routineId)
    curToken = currentToken()

    //If this is a function, parse and set its return type.
    if (routineId.getDefinition == DefinitionImpl.FUNCTION) {
      val variableDeclarationsParser = new VariableDeclarationsParser(this)
      variableDeclarationsParser.setDefinition(DefinitionImpl.FUNCTION)
      var `type` = variableDeclarationsParser.parseTypeSpec(curToken)

      curToken = currentToken()

      // The return type cannot be an array or record
      if (`type` != null) {
        val form = `type`.getForm
        if (form == TypeFormImpl.ARRAY || form == TypeFormImpl.RECORD) {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INVALID_TYPE, this)
        }
      }

      // Missing return type.
      else {
        `type` = Predefined.undefinedType
      }

      routineId.setTypeSpec(`type`)
      curToken = currentToken()
    }
  }

  def parseFormalParameters(toket: Token, routineId: SymTabEntry): Unit = {
    // Parse the formal parameters if there is an opening left parenthesis.
    var curToken = synchronize(DeclaredRoutineParser.LEFT_PAREN_SET)
    if (curToken.getTokenType == PascalTokenType.LEFT_PAREN) {
      curToken = nextToken() // consume (

      val parms = new util.ArrayList[SymTabEntry]()

      curToken = synchronize(DeclaredRoutineParser.PARAMETER_SET)
      var tokenType = curToken.getTokenType

      // Loop to parse sublists of formal parameter declarations.
      while (tokenType == PascalTokenType.IDENTIFIER || tokenType == PascalTokenType.VAR) {
        parms.addAll(parseParmSublist(curToken, routineId))
        curToken = currentToken()
        tokenType = curToken.getTokenType
      }

      // Closing right parenthesis.
      if (curToken.getTokenType == PascalTokenType.RIGHT_PAREN) {
        curToken = nextToken() // consume )
      } else {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_RIGHT_PAREN, this)
      }
      routineId.setAttribute(SymTabKeyImpl.ROUTINE_PARMS, parms)
    }
  }

  def parseParmSublist(toket: Token, routineId: SymTabEntry): util.ArrayList[SymTabEntry] = {
    var curToken = toket
    val isProgram = routineId.getDefinition == DefinitionImpl.PROGRAM
    var parmDefn = if (isProgram) DefinitionImpl.PROGRAM_PARM else null
    var tokenType = curToken.getTokenType

    // VAR or value parameter?
    if (tokenType == PascalTokenType.VAR) {
      if (!isProgram) {
        parmDefn = DefinitionImpl.VAR_PARM
      } else {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INVALID_VAR_PARM, this)
      }
      curToken = nextToken() // consume VAR
    } else if (!isProgram) {
      parmDefn = DefinitionImpl.VALUE_PARM
    }

    // Parse the parameter sublist and its type specification.
    val variableDeclarationsParser = new VariableDeclarationsParser(this)
    variableDeclarationsParser.setDefinition(parmDefn)
    val sublist = variableDeclarationsParser.parseIdentifierSublist(curToken, DeclaredRoutineParser.PARAMETER_FOLLOW_SET, DeclaredRoutineParser.COMMA_SET)
    curToken = currentToken()
    tokenType = curToken.getTokenType

    if (!isProgram) {
      // Look for one or more semicolons after a sublist.
      if (tokenType == PascalTokenType.SEMICOLON) {
        while (curToken.getTokenType == PascalTokenType.SEMICOLON) {
          curToken = nextToken() // consume the ;
        }
      }
      // If at the start of the next sublist, then missing a semicolon.
      else if (VariableDeclarationsParser.NEXT_START_SET.contains(tokenType)) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_SEMICOLON, this)
      }
      curToken = synchronize(DeclaredRoutineParser.PARAMETER_SET)
    }

    sublist
  }
}

object DeclaredRoutineParser {
  // Synchronization set for a formal parameter sublist.
  val PARAMETER_SET = DeclarationsParser.DECLARATION_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  // Synchronization set for the opening left parenthesis.
  val LEFT_PAREN_SET = DeclarationsParser.DECLARATION_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  PARAMETER_SET.add(PascalTokenType.VAR)
  PARAMETER_SET.add(PascalTokenType.IDENTIFIER)
  PARAMETER_SET.add(PascalTokenType.RIGHT_PAREN)
  // Synchronization set for the closing right parenthesis.
  val RIGHT_PAREN_SET = LEFT_PAREN_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  LEFT_PAREN_SET.add(PascalTokenType.LEFT_PAREN)
  LEFT_PAREN_SET.add(PascalTokenType.SEMICOLON)
  LEFT_PAREN_SET.add(PascalTokenType.COLON)
  // Synchronization set to follow a formal parameter identifier.
  val PARAMETER_FOLLOW_SET = new util.HashSet[PascalTokenType]()
  RIGHT_PAREN_SET.remove(PascalTokenType.LEFT_PAREN)
  RIGHT_PAREN_SET.add(PascalTokenType.RIGHT_PAREN)
  // Synchronization set for the , token.
  val COMMA_SET = new util.HashSet[PascalTokenType]()
  PARAMETER_FOLLOW_SET.add(PascalTokenType.COLON)
  PARAMETER_FOLLOW_SET.add(PascalTokenType.RIGHT_PAREN)
  PARAMETER_FOLLOW_SET.add(PascalTokenType.SEMICOLON)
  PARAMETER_FOLLOW_SET.addAll(DeclarationsParser.DECLARATION_START_SET)
  var dummyCounter = 0
  COMMA_SET.add(PascalTokenType.COMMA)
  COMMA_SET.add(PascalTokenType.COLON)
  COMMA_SET.add(PascalTokenType.IDENTIFIER)
  COMMA_SET.add(PascalTokenType.RIGHT_PAREN)
  COMMA_SET.add(PascalTokenType.SEMICOLON)
  COMMA_SET.addAll(DeclarationsParser.DECLARATION_START_SET)
}
