package frontend.pascal.parsers

import java.util

import frontend.Token
import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.{ICodeFactory, ICodeNode}

/**
  * For statement parser.
  *
  * @param pascalParserTD parent parser.
  */
class ForStatementParser(pascalParserTD: PascalParserTD) extends StatementParser(pascalParserTD) {

  /**
    * Parse a for statement.
    *
    * @param toket the initial token
    * @return the root of the generated parse tree.
    */
  override def parse(toket: Token): ICodeNode = {
    var curToken = nextToken() // consume the FOR
    val targetToken = curToken

    // Create the loop COMPOUND, LOOP, and TEST nodes.
    val compoundNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.COMPOUND)
    val loopNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.LOOP)
    val testNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.TEST)

    // Parse the embedded initial assignment.
    val assignmentStatementParser = new AssignmentStatementParser(this)
    val initAssignNode = assignmentStatementParser.parse(curToken)

    // Set the current line number attribute.
    setLineNumber(initAssignNode, targetToken) // TODO: why is targetToken the same in the next setLine function call?

    // The COMPOUND node adopts the initial ASSIGN and the LOOP nodes
    // as its first and second children.
    compoundNode.addChild(initAssignNode)
    compoundNode.addChild(loopNode)

    // Synchronize at the TO or DOWNTO.
    curToken = synchronize(ForStatementParser.TO_DOWNTO_SET)
    var direction = curToken.getTokenType

    // Look for the TO or DOWNTO.
    if ((direction == PascalTokenType.TO) || (direction == PascalTokenType.DOWNTO)) {
      curToken = nextToken() // consume the TO or DOWNTO.
    } else {
      direction = PascalTokenType.TO
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_TO_DOWNTO, this)
    }

    // Create a relationship operator node: GT for TO, or LT for DOWNTO.
    val relOpNode = ICodeFactory.createICodeNode(if (direction == PascalTokenType.TO) ICodeNodeTypeImpl.GT else ICodeNodeTypeImpl.LT)

    // Copy the control VARIABLE node. The relational operator
    //node adopts the copied VARIABLE node as its first child.
    val controlVarNode = initAssignNode.getChildren.get(0)
    relOpNode.addChild(controlVarNode.copy())

    // Parse the termination expression. The relational operator node
    // adopts the expression as its second child.
    val expressionParser = new ExpressionParser(this)
    relOpNode.addChild(expressionParser.parse(curToken))

    // The TEST node adopts the relational operator node as its only child.
    // The LOOP node adopts the TEST node as its first child.
    testNode.addChild(relOpNode)
    loopNode.addChild(testNode)

    // Synchronize at the DO.
    curToken = synchronize(ForStatementParser.DO_SET)
    if (curToken.getTokenType == PascalTokenType.DO) {
      curToken = nextToken() // consume the DO
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_DO, this)
    }

    // Parse the nested statement. The LOOP node adopts the statement
    // node as its second child.
    val statementParser = new StatementParser(this)
    loopNode.addChild(statementParser.parse(curToken))

    // Create an assignment with a copy of the control variable
    // to advance the value of the variable.
    val nextAssignNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.ASSIGN)
    nextAssignNode.addChild(controlVarNode.copy())

    // Create the arithmetic operator node:
    // ADD for TO, or SUBTRACT for DOWNTO.
    val arithOpNode = ICodeFactory.createICodeNode(if (direction == PascalTokenType.TO) ICodeNodeTypeImpl.ADD else ICodeNodeTypeImpl.SUBTRACT)

    // The operator node adopts a copy of the loop variable as its
    // first child and the value 1 as its second child.
    arithOpNode.addChild(controlVarNode.copy())
    val oneNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.INTEGER_CONSTANT)
    oneNode.setAttribute(ICodeKeyImpl.VALUE, 1)
    arithOpNode.addChild(oneNode)

    // The next ASSIGN node adopts the arithmetic operator node as its
    // second child. The loop node adopts the next ASSIGN node as its
    // third child.
    nextAssignNode.addChild(arithOpNode)
    loopNode.addChild(nextAssignNode)

    // Set the current line number attribute.
    setLineNumber(nextAssignNode, targetToken) // TODO: why is it the same as the previous one?

    compoundNode
  }
}

/**
  * Companion object.
  */
object ForStatementParser {
  val TO_DOWNTO_SET = ExpressionParser.EXPR_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  TO_DOWNTO_SET.add(PascalTokenType.TO)
  TO_DOWNTO_SET.add(PascalTokenType.DOWNTO)
  TO_DOWNTO_SET.addAll(StatementParser.STMT_FOLLOW_SET)

  val DO_SET = StatementParser.STMT_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  DO_SET.add(PascalTokenType.DO)
  DO_SET.addAll(StatementParser.STMT_FOLLOW_SET)
}