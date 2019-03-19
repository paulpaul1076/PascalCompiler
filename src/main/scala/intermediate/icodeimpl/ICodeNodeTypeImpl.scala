package intermediate.icodeimpl

import intermediate.ICodeNodeType

/**
  * Class defining node types.
  */
class ICodeNodeTypeImpl extends ICodeNodeType {

}

/**
  * Constants for all types of nodes.
  */
object ICodeNodeTypeImpl {
  // Program structure
  val PROGRAM = new ICodeNodeTypeImpl
  val PROCEDURE = new ICodeNodeTypeImpl
  val FUNCTION = new ICodeNodeTypeImpl

  // Statements
  val COMPOUND = new ICodeNodeTypeImpl
  val ASSIGN = new ICodeNodeTypeImpl
  val LOOP = new ICodeNodeTypeImpl
  val TEST = new ICodeNodeTypeImpl
  val CALL = new ICodeNodeTypeImpl
  val PARAMETERS = new ICodeNodeTypeImpl
  val IF = new ICodeNodeTypeImpl
  val SELECT = new ICodeNodeTypeImpl
  val SELECT_BRANCH = new ICodeNodeTypeImpl
  val SELECT_CONSTANTS = new ICodeNodeTypeImpl
  val NO_OP = new ICodeNodeTypeImpl

  // Relational operators
  val EQ = new ICodeNodeTypeImpl
  val NE = new ICodeNodeTypeImpl
  val LT = new ICodeNodeTypeImpl
  val GT = new ICodeNodeTypeImpl
  val LE = new ICodeNodeTypeImpl
  val GE = new ICodeNodeTypeImpl
  val NOT = new ICodeNodeTypeImpl

  // Multiplicative operators
  val MULTIPLY = new ICodeNodeTypeImpl
  val INTEGER_DIVIDE = new ICodeNodeTypeImpl
  val FLOAT_DIVIDE = new ICodeNodeTypeImpl
  val MOD = new ICodeNodeTypeImpl
  val AND = new ICodeNodeTypeImpl

  // Operands
  val VARIABLE = new ICodeNodeTypeImpl
  val SUBSCRIPTS = new ICodeNodeTypeImpl
  val FIELD = new ICodeNodeTypeImpl
  val INTEGER_CONSTANT = new ICodeNodeTypeImpl
  val REAL_CONSTANT = new ICodeNodeTypeImpl
  val STRING_CONSTANT = new ICodeNodeTypeImpl
  val BOOLEAN_CONSTANT = new ICodeNodeTypeImpl
}