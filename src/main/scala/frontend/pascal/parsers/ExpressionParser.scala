package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{Parser, Token, TokenType}
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.{ICodeFactory, ICodeNode, ICodeNodeType}


// TODO: find out if this should be a subclass of StatementParser.
//  I think it shouldn't be, because statements and expressions are 2 different things.

/**
  * Class with the expression parsing logic.
  *
  * @param pascalParser parent parser.
  */
class ExpressionParser(pascalParser: PascalParserTD) extends StatementParser(pascalParser) {

  /**
    * Parses the expression.
    *
    * @param token the initial token.
    * @return parsed subtree.
    */
  override def parse(token: Token): ICodeNode = parseExpression(token)

  /**
    * Parses the expression.
    *
    * @param toket the initial token.
    * @return parsed subtree.
    */
  def parseExpression(toket: Token): ICodeNode = {
    var curToken = toket
    var rootNode = parseSimpleExpression(curToken)

    curToken = currentToken()
    val tokenType = curToken.getTokenType

    // Look for a relational operator.
    if (ExpressionParser.REL_OPS.contains(tokenType)) {

      // Create a new operator node and adopt the current tree
      // as its first child.
      val nodeType = ExpressionParser.REL_OPS_MAP.get(tokenType)
      val opNode = ICodeFactory.createICodeNode(nodeType)
      opNode.addChild(rootNode)

      curToken = nextToken() // consume the operator

      // Parse the second simple expression. The operator node adopts
      // the simple expression's tree as its second child.
      opNode.addChild(parseSimpleExpression(curToken))

      // The operator node becomes the new root node.
      rootNode = opNode
    }
    rootNode
  }

  /**
    * Parse a simple expression.
    *
    * @param toket the initial token.
    * @return the root of the generated parse subtree.
    */
  private def parseSimpleExpression(toket: Token): ICodeNode = {
    var signType: TokenType = null // type of leading sign (if any)
    var curToken = toket
    // Look for a leading + or - sign.
    var tokenType = curToken.getTokenType
    if ((tokenType == PascalTokenType.PLUS) || (tokenType == PascalTokenType.MINUS)) {
      signType = tokenType
      curToken = nextToken() // consume the + or -
    }

    // Parse a term and make the root of its tree the root node.
    var rootNode = parseTerm(curToken)

    // Was there a leading - sign?
    if (signType == PascalTokenType.MINUS) {
      // Create a NEGATE node and adopt the current tree
      // as its child. The NEGATE node becomes the new root node.
      val negateNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.NEGATE)
      negateNode.addChild(rootNode)
      rootNode = negateNode
    }

    curToken = currentToken()
    tokenType = curToken.getTokenType

    // Loop over additive operators.
    while (ExpressionParser.ADD_OPS.contains(tokenType)) {
      // Create a new operator node and adopt the current tree
      // as its first child
      val nodeType = ExpressionParser.ADD_OPS_MAP.get(tokenType)
      val opNode = ICodeFactory.createICodeNode(nodeType)
      opNode.addChild(rootNode)
      curToken = nextToken() // consume the operator

      // Parse another term. The operator node adopts
      // the term's tree as its second child.
      opNode.addChild(parseTerm(curToken))

      // The operator node becomes the new root node.
      rootNode = opNode

      curToken = currentToken()
      tokenType = curToken.getTokenType
    }

    rootNode
  }

  /**
    * Parse term function.
    *
    * @param toket the current token.
    * @return the tree corresponding to this term.
    */
  private def parseTerm(toket: Token): ICodeNode = {
    var curToken = toket
    var rootNode = parseFactor(curToken)

    curToken = currentToken()
    var tokenType = curToken.getTokenType

    // Loop over multiplicative operators.
    while (ExpressionParser.MULT_OPS.contains(tokenType)) {
      // Create a new operator node and adopt the current tree
      // as its first child.
      val nodeType = ExpressionParser.MULT_OPS_MAP.get(tokenType)
      val opNode = ICodeFactory.createICodeNode(nodeType)

      opNode.addChild(rootNode)

      curToken = nextToken() // consume the operator

      // Parse another factor. The operator node adopts
      // the term's tree as its second child.
      opNode.addChild(parseFactor(curToken))

      // The operator node becomes the new root node.
      rootNode = opNode

      curToken = currentToken()
      tokenType = curToken.getTokenType
    }

    rootNode
  }

  /**
    * Parse factor function.
    *
    * @param toket current token.
    * @return a parse tree, corresponding to this factor.
    */
  private def parseFactor(toket: Token): ICodeNode = {
    var curToken = toket
    val tokenType = curToken.getTokenType
    var rootNode: ICodeNode = null

    tokenType.asInstanceOf[PascalTokenType] match {
      case PascalTokenType.IDENTIFIER =>
        // Look up the identifier in the symbol table stack.
        // Flag the identifier as undefined if it's not found.
        val name = curToken.getText.toLowerCase
        var id = Parser.symTabStack.lookup(name)
        if (id == null) {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.IDENTIFIER_UNDEFINED, this)
          id = Parser.symTabStack.enterLocal(name)
        }

        rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.VARIABLE)
        rootNode.setAttribute(ICodeKeyImpl.ID, id)
        id.appendLineNumber(curToken.getLineNumber)

        curToken = nextToken()
      case PascalTokenType.INTEGER =>
        // Create an INTEGER_CONSTANT node as the root node.
        rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.INTEGER_CONSTANT)
        rootNode.setAttribute(ICodeKeyImpl.VALUE, curToken.getValue)

        curToken = nextToken()
      case PascalTokenType.REAL =>
        // Create a REAL_CONSTANT node as the root node.
        rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.REAL_CONSTANT)
        rootNode.setAttribute(ICodeKeyImpl.VALUE, curToken.getValue)

        curToken = nextToken()
      case PascalTokenType.STRING =>
        val value = curToken.getValue.asInstanceOf[String]

        // Create a STRING_CONSTANT node as the root node.
        rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.STRING_CONSTANT)
        rootNode.setAttribute(ICodeKeyImpl.VALUE, value)

        curToken = nextToken()
      case PascalTokenType.NOT =>
        curToken = nextToken() // consume the NOT

        // Create a NOT node as the root node.
        rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.NOT)

        // Parse the factor. The NOT node adopts the
        // factory node as its child.
        rootNode.addChild(parseFactor(curToken))
      case PascalTokenType.LEFT_PAREN =>
        curToken = nextToken() // consume the (

        // Parse an expression and make its node the root node.
        rootNode = parseExpression(curToken)

        // Look for the matching ) token.
        curToken = currentToken()
        if (curToken.getTokenType == PascalTokenType.RIGHT_PAREN) {
          curToken = nextToken()
        } else {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_RIGHT_PAREN, this)
        }
      case _ => PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.UNEXPECTED_TOKEN, this)
    }

    rootNode
  }
}

/**
  * Companion object.
  */
private object ExpressionParser {

  val EXPR_START_SET = new util.HashSet[PascalTokenType]()
  EXPR_START_SET.add(PascalTokenType.MINUS)
  EXPR_START_SET.add(PascalTokenType.PLUS)
  EXPR_START_SET.add(PascalTokenType.IDENTIFIER)
  EXPR_START_SET.add(PascalTokenType.INTEGER)
  EXPR_START_SET.add(PascalTokenType.REAL)
  EXPR_START_SET.add(PascalTokenType.STRING)
  EXPR_START_SET.add(PascalTokenType.NOT)
  EXPR_START_SET.add(PascalTokenType.LEFT_PAREN)


  // ----- parseExpression
  /**
    * Set of relational operators.
    */
  val REL_OPS = new util.HashSet[PascalTokenType]()
  REL_OPS.add(PascalTokenType.EQUALS)
  REL_OPS.add(PascalTokenType.NOT_EQUALS)
  REL_OPS.add(PascalTokenType.LESS_THAN)
  REL_OPS.add(PascalTokenType.LESS_EQUALS)
  REL_OPS.add(PascalTokenType.GREATER_THAN)
  REL_OPS.add(PascalTokenType.GREATER_EQUALS)

  /**
    * Map relational operator tokens to node types.
    */
  val REL_OPS_MAP = new util.HashMap[PascalTokenType, ICodeNodeType]()
  REL_OPS_MAP.put(PascalTokenType.EQUALS, ICodeNodeTypeImpl.EQ)
  REL_OPS_MAP.put(PascalTokenType.NOT_EQUALS, ICodeNodeTypeImpl.NE)
  REL_OPS_MAP.put(PascalTokenType.LESS_THAN, ICodeNodeTypeImpl.LT)
  REL_OPS_MAP.put(PascalTokenType.LESS_EQUALS, ICodeNodeTypeImpl.LE)
  REL_OPS_MAP.put(PascalTokenType.GREATER_THAN, ICodeNodeTypeImpl.GT)
  REL_OPS_MAP.put(PascalTokenType.GREATER_EQUALS, ICodeNodeTypeImpl.GE)

  // ----- parseSimpleExpression
  /**
    * Set of additive operators.
    */
  val ADD_OPS = new util.HashSet[PascalTokenType]()
  ADD_OPS.add(PascalTokenType.PLUS)
  ADD_OPS.add(PascalTokenType.MINUS)
  ADD_OPS.add(PascalTokenType.OR)

  /**
    * Map additive operator tokens to node types.
    */
  val ADD_OPS_MAP = new util.HashMap[PascalTokenType, ICodeNodeTypeImpl]()
  ADD_OPS_MAP.put(PascalTokenType.PLUS, ICodeNodeTypeImpl.ADD)
  ADD_OPS_MAP.put(PascalTokenType.MINUS, ICodeNodeTypeImpl.SUBTRACT)
  ADD_OPS_MAP.put(PascalTokenType.OR, ICodeNodeTypeImpl.OR)

  // ----- parseTerm
  /**
    * Set of multiplicative operators.
    */
  val MULT_OPS = new util.HashSet[PascalTokenType]()
  MULT_OPS.add(PascalTokenType.STAR)
  MULT_OPS.add(PascalTokenType.SLASH)
  MULT_OPS.add(PascalTokenType.DIV)
  MULT_OPS.add(PascalTokenType.MOD)
  MULT_OPS.add(PascalTokenType.AND)

  /**
    * Map multiplicative operators to the appropriate node types.
    */
  val MULT_OPS_MAP = new util.HashMap[PascalTokenType, ICodeNodeType]()
  MULT_OPS_MAP.put(PascalTokenType.STAR, ICodeNodeTypeImpl.MULTIPLY)
  MULT_OPS_MAP.put(PascalTokenType.SLASH, ICodeNodeTypeImpl.FLOAT_DIVIDE)
  MULT_OPS_MAP.put(PascalTokenType.DIV, ICodeNodeTypeImpl.INTEGER_DIVIDE)
  MULT_OPS_MAP.put(PascalTokenType.MOD, ICodeNodeTypeImpl.MOD)
  MULT_OPS_MAP.put(PascalTokenType.AND, ICodeNodeTypeImpl.AND)
}