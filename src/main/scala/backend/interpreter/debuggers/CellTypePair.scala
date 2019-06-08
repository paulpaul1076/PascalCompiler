package backend.interpreter.debuggers

import java.util

import backend.interpreter.{Cell, Debugger}
import frontend.pascal.PascalTokenType
import intermediate.symtabimpl.Predefined
import intermediate.{SymTab, SymTabEntry, TypeSpec}
import intermediate.typeimpl.{TypeFormImpl, TypeKeyImpl}

/**
  * Memory cell and data type pair used by the debugger.
  *
  * Used for parsing variables in debugger's input, a variable can have subscripts and fields specified,
  * so this class parses all that.
  *
  * @param `typee`   type of the variable.
  * @param celll     memory cell.
  * @param debuggerr the parent debugger.
  */
class CellTypePair(`typee`: TypeSpec, celll: Cell, debuggerr: Debugger) {

  private var `type` = `typee`
  private var cell = celll
  private val debugger = debuggerr

  parseVariable()

  // Getters.
  def getType: TypeSpec = `type`

  def getCell: Cell = cell


  /**
    * Parses a variable that can have subscripts and fields specified.
    */
  def parseVariable(): Unit = {
    var form = `type`.getForm
    var value = cell.getValue

    // Loop to process array subscripts and record fields.
    while (CellTypePair.MODIFIER_SET.contains(debugger.currentToken().getTokenType)) {
      if (form == TypeFormImpl.ARRAY) {
        parseArrayVariable(value.asInstanceOf[Array[Cell]])
      } else if (form == TypeFormImpl.RECORD) {
        parseRecordVariable(value.asInstanceOf[util.HashMap[String, Cell]])
      }

      value = cell.getValue
      form = `type`.getForm
    }
  }

  /**
    * parses array subscripts.
    *
    * @param array array
    */
  def parseArrayVariable(array: Array[Cell]): Unit = {
    debugger.nextToken()

    val index = debugger.getInteger("Integer index expected.")
    var minValue = 0
    val indexType = `type`.getAttribute(TypeKeyImpl.ARRAY_INDEX_TYPE).asInstanceOf[TypeSpec]

    rangeCheck(index, indexType, "Index out of range")
    `type` = `type`.getAttribute(TypeKeyImpl.ARRAY_ELEMENT_TYPE).asInstanceOf[TypeSpec]

    if (indexType.getForm == TypeFormImpl.SUBRANGE) {
      minValue = indexType.getAttribute(TypeKeyImpl.SUBRANGE_MIN_VALUE).asInstanceOf[Integer]
    }

    cell = array(index - minValue)

    if (debugger.currentToken().getTokenType == PascalTokenType.RIGHT_BRACKET) {
      debugger.nextToken()
    } else {
      throw new Exception("] expected.")
    }
  }

  /**
    * Parses a field of a variable.
    *
    * @param record record that contains the field.
    */
  def parseRecordVariable(record: util.HashMap[String, Cell]): Unit = {
    debugger.nextToken()

    val fieldName = debugger.getWord("Field name expected.")

    if (record.containsKey(fieldName)) {
      cell = record.get(fieldName)
    } else {
      throw new Exception("Invalid field name.")
    }

    val symTab = `type`.getAttribute(TypeKeyImpl.RECORD_SYMTAB).asInstanceOf[SymTab]
    val id = symTab.lookup(fieldName)
    `type` = id.getTypeSpec
  }

  /**
    * Checks that an index is in range.
    *
    * @param value        the index value.
    * @param `type`       the type of subscripts.
    * @param errorMessage erroro message to be printed if something went wrong.
    */
  private def rangeCheck(value: Int, `type`: TypeSpec, errorMessage: String): Unit = {
    val form = `type`.getForm
    var minValue: Integer = null
    var maxValue: Integer = null

    if (form == TypeFormImpl.SUBRANGE) {
      minValue = `type`.getAttribute(TypeKeyImpl.SUBRANGE_MIN_VALUE).asInstanceOf[Integer]
      maxValue = `type`.getAttribute(TypeKeyImpl.SUBRANGE_MAX_VALUE).asInstanceOf[Integer]
    } else if (form == TypeFormImpl.ENUMERATION) {
      val constants = `type`.getAttribute(TypeKeyImpl.ENUMERATION_CONSTANTS).asInstanceOf[util.ArrayList[SymTabEntry]]
      minValue = 0
      maxValue = constants.size() - 1
    }

    if (minValue != null && (value < minValue || value > maxValue)) {
      throw new Exception(errorMessage)
    }
  }

  /**
    * Resets cell's value.
    *
    * @param value the new value.
    */
  def setValue(value: Any): Unit = {
    if (`type`.baseType == Predefined.integerType && value.isInstanceOf[Integer]
      || `type` == Predefined.realType && value.isInstanceOf[java.lang.Float]
      || `type` == Predefined.booleanType && value.isInstanceOf[java.lang.Boolean]
      || `type` == Predefined.charType && value.isInstanceOf[Character]) {
      if (`type`.baseType == Predefined.integerType) {
        rangeCheck(value.asInstanceOf[Integer], `type`, "Value out of range.")
      }
      cell.setValue(value)
    } else {
      throw new Exception("Type mismatch.")
    }
  }
}

/**
  * Companion object with "static" vars.
  */
object CellTypePair {
  val MODIFIER_SET = new util.HashSet[PascalTokenType]()
  MODIFIER_SET.add(PascalTokenType.LEFT_BRACKET)
  MODIFIER_SET.add(PascalTokenType.DOT)
}