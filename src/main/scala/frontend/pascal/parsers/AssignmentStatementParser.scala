package frontend.pascal.parsers

import java.util

import frontend.Token
import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import intermediate.icodeimpl.ICodeNodeTypeImpl
import intermediate.symtabimpl.Predefined
import intermediate.typeimpl.TypeChecker
import intermediate.{ICodeFactory, ICodeNode}

/**
  * Assignment parser.
  *
  * @param pascalParser parent parser.
  */
class AssignmentStatementParser(pascalParser: PascalParserTD) extends StatementParser(pascalParser) {
  /**
    * Parse an assignment statement.
    *
    * @param toket the initial token.
    * @return the root of the generated parse tree.
    */
  override def parse(toket: Token): ICodeNode = {
    // Create an ASSIGN node.
    val assignNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.ASSIGN)
    var curToken = toket

    //Parse the target variable.
    val variableParser = new VariableParser(this)
    val targetNode = variableParser.parse(curToken)
    val targetType = if (targetNode != null) targetNode.getTypeSpec else Predefined.undefinedType

    // The ASSIGN node adopts the variable node as its first child.
    assignNode.addChild(targetNode)

    //Synchronize on the := token.
    curToken = synchronize(AssignmentStatementParser.COLON_EQUALS_SET)

    // Look for the := token.
    if (curToken.getTokenType == PascalTokenType.COLON_EQUALS) {
      curToken = nextToken() // consume the :=
    } else {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_COLON_EQUALS, this)
    }

    // Parse the expression. The ASSIGN node adopts the expression's
    // node as its second child.
    val expressionParser = new ExpressionParser(this)
    val exprNode = expressionParser.parse(curToken)
    assignNode.addChild(exprNode)

    // Type check: Assignment compatible?
    val exprType = if (exprNode != null) exprNode.getTypeSpec else Predefined.undefinedType
    if (!TypeChecker.areAssignmentCompatible(targetType, exprType)) {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
    }
    assignNode.setTypeSpec(targetType)

    assignNode
  }
}

/**
 * Companion object.
 */
private object AssignmentStatementParser {
  val COLON_EQUALS_SET: util.HashSet[PascalTokenType] = ExpressionParser.EXPR_START_SET.clone().asInstanceOf[util.HashSet[PascalTokenType]]
  COLON_EQUALS_SET.add(PascalTokenType.COLON_EQUALS)
  COLON_EQUALS_SET.addAll(StatementParser.STMT_FOLLOW_SET)
}