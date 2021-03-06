package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{EofToken, Parser, Token}
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.symtabimpl.DefinitionImpl
import intermediate.{ICodeFactory, ICodeNode}

/**
 * Statement parser.
 *
 * @param pascalParser parent parser.
 */
class StatementParser(pascalParser: PascalParserTD) extends PascalParserTD(pascalParser) {

  /**
   * Parses a list of statements.
   *
   * @param toket      the starting token.
   * @param parentNode parent node of this statement's node.
   * @param terminator the token type of ndoe that terminates the list.
   * @param errorCode  errorCode if the terminator token is missing.
   */
  def parseList(toket: Token, parentNode: ICodeNode, terminator: PascalTokenType, errorCode: PascalErrorCode): Unit = {
    // Synchronization set for the terminator.
    val terminatorSet = StatementParser.STMT_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
    terminatorSet.add(terminator)

    // Loop to parse each statement until the END token.
    // or the end of the source file.
    var curToken = toket // because we can't assign anything to "token"
    while (!curToken.isInstanceOf[EofToken] && (curToken.getTokenType != terminator)) {
      //Parse a statement. The parent node adopts the statement node.
      val statementNode = parse(curToken)
      parentNode.addChild(statementNode)
      curToken = currentToken()
      val tokenType = curToken.getTokenType

      // Look for the semicolon between statements.
      if (tokenType == PascalTokenType.SEMICOLON) {
        curToken = nextToken() // consume the ;
      }
      // If at the start of the next assignment statement,
      // then missing a semicolon.
      else if (StatementParser.STMT_START_SET.contains(tokenType)) {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_SEMICOLON, this)
      }

      // Synchronize at the start of the next statement
      // or at the terminator.
      curToken = synchronize(terminatorSet)
    }

    // Look for the terminator token.
    if (curToken.getTokenType == terminator) {
      curToken = nextToken() // consume the terminator token
    } else {
      PascalParserTD.errorHandler.flag(curToken, errorCode, this)
    }
  }

  // TODO: Should this be a protected method?

  /**
   * Parse a statement. To be overridden by the specialized statement parser subclasses.
   *
    * @param toket the initial token
   * @return the root of the generated parse tree.
   */
  def parse(toket: Token): ICodeNode = {
    var curToken = toket
    val statementNode = curToken.getTokenType match {
      case PascalTokenType.BEGIN      =>
        val compoundStatementParser = new CompoundStatementParser(this)
        compoundStatementParser.parse(curToken)
      case PascalTokenType.IDENTIFIER =>
        val name = curToken.getText.toLowerCase
        val id = Parser.symTabStack.lookup(name)
        val idDefn = if (id != null) id.getDefinition else DefinitionImpl.UNDEFINED

        // Assignment statement or procedure call.
        idDefn.asInstanceOf[DefinitionImpl] match {
          case DefinitionImpl.VARIABLE | DefinitionImpl.VALUE_PARM | DefinitionImpl.VAR_PARM | DefinitionImpl.UNDEFINED =>
            val assignmentStatementParser = new AssignmentStatementParser(this)
            assignmentStatementParser.parse(curToken)
          case DefinitionImpl.FUNCTION => // TODO: how does a function get called without an assignment statement
            val assignmentStatementParser = new AssignmentStatementParser(this)
            assignmentStatementParser.parseFunctionNameAssignment(curToken)
          case DefinitionImpl.PROCEDURE =>
            val callParser = new CallParser(this)
            callParser.parse(curToken)
          case _ =>
            PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.UNEXPECTED_TOKEN, this)
            curToken = nextToken() // consume identifier
            null
        }

      case PascalTokenType.REPEAT     =>
        val repeatStatementParser = new RepeatStatementParser(this)
        repeatStatementParser.parse(curToken)
      case PascalTokenType.WHILE      =>
        val whileStatementParser = new WhileStatementParser(this)
        whileStatementParser.parse(curToken)
      case PascalTokenType.FOR        =>
        val forStatementParser = new ForStatementParser(this)
        forStatementParser.parse(curToken)
      case PascalTokenType.IF         =>
        val ifStatementParser = new IfStatementParser(this)
        ifStatementParser.parse(curToken)
      case PascalTokenType.CASE       =>
        val caseStatementParser = new CaseStatementParser(this)
        caseStatementParser.parse(curToken)
      case _                          =>
        ICodeFactory.createICodeNode(ICodeNodeTypeImpl.NO_OP)
    }

    setLineNumber(statementNode, curToken)
    statementNode
  }

  /**
   * Set the current line number as a statement node attribute.
   *
   * @param node  ICodeNode.
   * @param token the initial token.
   */
  protected def setLineNumber(node: ICodeNode, token: Token): Unit = {
    if (node != null) {
      node.setAttribute(ICodeKeyImpl.LINE, token.getLineNumber)
    }
  }
}

/**
 * Companion object.
 */
private object StatementParser {
  /**
   * Synchronization set for starting a statement.
   */
  val STMT_START_SET = new util.HashSet[PascalTokenType]()
  STMT_START_SET.add(PascalTokenType.BEGIN)
  STMT_START_SET.add(PascalTokenType.CASE)
  STMT_START_SET.add(PascalTokenType.FOR)
  STMT_START_SET.add(PascalTokenType.IF)
  STMT_START_SET.add(PascalTokenType.REPEAT)
  STMT_START_SET.add(PascalTokenType.WHILE)
  STMT_START_SET.add(PascalTokenType.IDENTIFIER)
  STMT_START_SET.add(PascalTokenType.SEMICOLON) // TODO: this is here in order to "handle the empty statement", what does that mean?

  /**
   * Synchronization set for following a statement.
   */
  val STMT_FOLLOW_SET = new util.HashSet[PascalTokenType]()
  STMT_FOLLOW_SET.add(PascalTokenType.SEMICOLON)
  STMT_FOLLOW_SET.add(PascalTokenType.END)
  STMT_FOLLOW_SET.add(PascalTokenType.ELSE)
  STMT_FOLLOW_SET.add(PascalTokenType.UNTIL)
  STMT_FOLLOW_SET.add(PascalTokenType.DOT)
}