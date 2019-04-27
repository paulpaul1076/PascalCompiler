package frontend.pascal.parsers

import java.util

import frontend.pascal.{PascalErrorCode, PascalParserTD, PascalTokenType}
import frontend.{Parser, Token, TokenType}
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.symtabimpl.{DefinitionImpl, Predefined, SymTabKeyImpl}
import intermediate.typeimpl.TypeChecker
import intermediate.{ICodeFactory, ICodeNode, ICodeNodeType, TypeFactory}


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

    var resultType = if (rootNode != null) rootNode.getTypeSpec else Predefined.undefinedType

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
      val simExprNode = parseSimpleExpression(curToken)
      opNode.addChild(simExprNode)

      // The operator node becomes the new root node.
      rootNode = opNode

      // Type check: The operands must be comparison compatible.
      val simExprType = if (simExprNode != null) simExprNode.getTypeSpec else Predefined.undefinedType

      if (TypeChecker.areComparisonCompatible(resultType, simExprType)) {
        resultType = Predefined.booleanType
      } else {
        PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
        resultType = Predefined.undefinedType
      }
    }

    if (rootNode != null) {
      rootNode.setTypeSpec(resultType)
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
    var signToken: Token = null
    // Look for a leading + or - sign.
    var tokenType = curToken.getTokenType
    if ((tokenType == PascalTokenType.PLUS) || (tokenType == PascalTokenType.MINUS)) {
      signType = tokenType
      signToken = curToken
      curToken = nextToken() // consume the + or -
    }

    // Parse a term and make the root of its tree the root node.
    var rootNode = parseTerm(curToken)
    var resultType = if (rootNode != null) rootNode.getTypeSpec else Predefined.undefinedType

    // Type check : Leading sign.
    if (signType != null && !TypeChecker.isIntegerOrReal(resultType)) {
      PascalParserTD.errorHandler.flag(signToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
    }

    // Was there a leading - sign?
    if (signType == PascalTokenType.MINUS) {
      // Create a NEGATE node and adopt the current tree
      // as its child. The NEGATE node becomes the new root node.
      val negateNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.NEGATE)
      negateNode.addChild(rootNode)
      negateNode.setTypeSpec(rootNode.getTypeSpec)
      rootNode = negateNode
    }

    curToken = currentToken()
    tokenType = curToken.getTokenType

    // Loop over additive operators.
    while (ExpressionParser.ADD_OPS.contains(tokenType)) {
      val operator = tokenType
      // Create a new operator node and adopt the current tree
      // as its first child
      val nodeType = ExpressionParser.ADD_OPS_MAP.get(operator)
      val opNode = ICodeFactory.createICodeNode(nodeType)
      opNode.addChild(rootNode)
      curToken = nextToken() // consume the operator

      // Parse another term. The operator node adopts
      // the term's tree as its second child.
      val termNode = parseTerm(curToken)
      opNode.addChild(termNode)
      val termType = if (termNode != null) termNode.getTypeSpec else Predefined.undefinedType

      // The operator node becomes the new root node.
      rootNode = opNode

      // Determine the result type.
      operator.asInstanceOf[PascalTokenType] match {
        case PascalTokenType.PLUS | PascalTokenType.MINUS =>
          // Both operands integer ==> integer result
          if (TypeChecker.areBothInteger(resultType, termType)) {
            resultType = Predefined.integerType
          }
          // Both real operands or one real and one integer operand
          // ==> real result.
          else if (TypeChecker.isAtLeastOneReal(resultType, termType)) {
            resultType = Predefined.realType
          } else {
            PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
          }
        case PascalTokenType.OR =>
          // Both operands boolean ==> boolean result.
          if (TypeChecker.areBothBoolean(resultType, termType)) {
            resultType = Predefined.booleanType
          } else {
            PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
          }
      }

      rootNode.setTypeSpec(resultType)

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
    var resultType = if (rootNode != null) rootNode.getTypeSpec else Predefined.undefinedType

    curToken = currentToken()
    var tokenType = curToken.getTokenType

    // Loop over multiplicative operators.
    while (ExpressionParser.MULT_OPS.contains(tokenType)) {
      val operator = tokenType
      // Create a new operator node and adopt the current tree
      // as its first child.
      val nodeType = ExpressionParser.MULT_OPS_MAP.get(operator)
      val opNode = ICodeFactory.createICodeNode(nodeType)

      opNode.addChild(rootNode)

      curToken = nextToken() // consume the operator

      // Parse another factor. The operator node adopts
      // the term's tree as its second child.
      val factorNode = parseFactor(curToken)
      opNode.addChild(factorNode)

      // The operator node becomes the new root node.
      rootNode = opNode

      curToken = currentToken()
      tokenType = curToken.getTokenType

      val factorType = if (factorNode != null) factorNode.getTypeSpec else Predefined.undefinedType

      // Determine the result type.
      operator.asInstanceOf[PascalTokenType] match {
        case PascalTokenType.STAR =>
          // Both operands integer ==> integer result.
          if (TypeChecker.areBothInteger(resultType, factorType)) {
            resultType = Predefined.integerType
          } // Both real operands or one real and one integer operand. ==> real result
          else if (TypeChecker.isAtLeastOneReal(resultType, factorType)) {
            resultType = Predefined.realType
          } else {
            PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
          }
        case PascalTokenType.SLASH =>
          // Al integer and real operand combinations
          // ==> real result.
          if (TypeChecker.areBothInteger(resultType, factorType) || TypeChecker.isAtLeastOneReal(resultType, factorType)) {
            resultType = Predefined.realType
          } else {
            PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
          }
        case PascalTokenType.DIV | PascalTokenType.MOD =>
          // Both operands integer ==> integer result.
          if (TypeChecker.areBothInteger(resultType, factorType)) {
            resultType = Predefined.integerType
          } else {
            PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
          }
        case PascalTokenType.AND =>
          // Both operands boolean ==> boolean result.
          if (TypeChecker.areBothBoolean(resultType, factorType)) {
            resultType = Predefined.booleanType
          } else {
            PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
          }
      }

      rootNode.setTypeSpec(resultType)

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
        rootNode = parseIdentifier(curToken)
      case PascalTokenType.INTEGER =>
        // Create an INTEGER_CONSTANT node as the root node.
        rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.INTEGER_CONSTANT)
        rootNode.setAttribute(ICodeKeyImpl.VALUE, curToken.getValue)

        curToken = nextToken() // consume the number

        rootNode.setTypeSpec(Predefined.integerType)
      case PascalTokenType.REAL =>
        // Create a REAL_CONSTANT node as the root node.
        rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.REAL_CONSTANT)
        rootNode.setAttribute(ICodeKeyImpl.VALUE, curToken.getValue)

        curToken = nextToken() // consume the number

        rootNode.setTypeSpec(Predefined.realType)
      case PascalTokenType.STRING =>
        val value = curToken.getValue.asInstanceOf[String]

        // Create a STRING_CONSTANT node as the root node.
        rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.STRING_CONSTANT)
        rootNode.setAttribute(ICodeKeyImpl.VALUE, value)

        val resultType = if (value.length == 1) Predefined.charType else TypeFactory.createStringType(value)

        curToken = nextToken() // consume the string

        rootNode.setTypeSpec(resultType)
      case PascalTokenType.NOT =>
        curToken = nextToken() // consume the NOT

        // Create a NOT node as the root node.
        rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.NOT)

        // Parse the factor. The NOT node adopts the
        // factor node as its child.
        val factorNode = parseFactor(curToken)
        rootNode.addChild(factorNode)

        // Type check: the factor must be boolean.
        val factorType = if (factorNode != null) factorNode.getTypeSpec else Predefined.undefinedType
        if (!TypeChecker.isBoolean(factorType)) {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.INCOMPATIBLE_TYPES, this)
        }

        rootNode.setTypeSpec(Predefined.booleanType)
      case PascalTokenType.LEFT_PAREN =>
        curToken = nextToken() // consume the (

        // Parse an expression and make its node the root node.
        rootNode = parseExpression(curToken)
        val resultType = if (rootNode != null) rootNode.getTypeSpec else Predefined.undefinedType // TODO: why is this useless crap here?

        // Look for the matching ) token.
        curToken = currentToken()
        if (curToken.getTokenType == PascalTokenType.RIGHT_PAREN) {
          curToken = nextToken()
        } else {
          PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.MISSING_RIGHT_PAREN, this)
        }

        rootNode.setTypeSpec(resultType) // TODO: why is this useless crap here?
      case _ => PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.UNEXPECTED_TOKEN, this)
    }

    rootNode
  }

  private def parseIdentifier(toket: Token): ICodeNode = {
    var rootNode: ICodeNode = null

    var curToken = toket
    // Look up the identifier in the symbol table stack.
    val name = curToken.getText.toLowerCase
    var id = Parser.symTabStack.lookup(name)

    // Undefined.
    if (id == null) {
      PascalParserTD.errorHandler.flag(curToken, PascalErrorCode.IDENTIFIER_UNDEFINED, this)
      id = Parser.symTabStack.enterLocal(name)
      id.setDefinition(DefinitionImpl.UNDEFINED)
      id.setTypeSpec(Predefined.undefinedType)
    }

    val defnCode = id.getDefinition

    defnCode.asInstanceOf[DefinitionImpl] match {
      case DefinitionImpl.CONSTANT =>
        val value = id.getAttribute(SymTabKeyImpl.CONSTANT_VALUE)
        val `type` = id.getTypeSpec

        if (value.isInstanceOf[Int]) {
          rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.INTEGER_CONSTANT)
          rootNode.setAttribute(ICodeKeyImpl.VALUE, value)
        } else if (value.isInstanceOf[Float]) {
          rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.REAL_CONSTANT)
          rootNode.setAttribute(ICodeKeyImpl.VALUE, value)
        } else if (value.isInstanceOf[String]) {
          rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.STRING_CONSTANT)
          rootNode.setAttribute(ICodeKeyImpl.VALUE, value)
        }

        id.appendLineNumber(curToken.getLineNumber)
        curToken = nextToken() // consume the constant identifier

        if (rootNode != null) {
          rootNode.setTypeSpec(`type`)
        }
      case DefinitionImpl.ENUMERATION_CONSTANT =>
        val value = id.getAttribute(SymTabKeyImpl.CONSTANT_VALUE)
        val `type` = id.getTypeSpec

        rootNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.INTEGER_CONSTANT)
        rootNode.setAttribute(ICodeKeyImpl.VALUE, value)

        id.appendLineNumber(curToken.getLineNumber)
        curToken = nextToken() // consume the enum constant identifier

        rootNode.setTypeSpec(`type`)
      case _ =>
        val variableParser = new VariableParser(this)
        rootNode = variableParser.parse(curToken, id)
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