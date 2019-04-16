package util

import java.util

import intermediate.symtabimpl.{DefinitionImpl, SymTabKeyImpl}
import intermediate.typeimpl.{TypeFormImpl, TypeKeyImpl}
import intermediate.{SymTab, SymTabEntry, SymTabStack, TypeSpec}

import scala.collection.JavaConverters._

/**
  * Generate a cross-reference listing.
  */
class CrossReferencer {

  /**
    * Print the cross-reference table.
    *
    * @param symTabStack the symbol table stack.
    */
  def print(symTabStack: SymTabStack): Unit = {
    println("\n===== CROSS-REFERENCE TABLE =====")

    val programId = symTabStack.getProgramId()
    printRoutine(programId)
  }

  /**
    * Print a cross-reference table for a routine.
    *
    * @param routineId the routine identifier's symbol table entry.
    */
  private def printRoutine(routineId: SymTabEntry): Unit = {
    val definition = routineId.getDefinition
    println(s"\n*** ${definition.toString} ${routineId.getName} ***")
    printColumnHeadings()

    // Print the entries in the routine's symbol table.
    val symTab = routineId.getAttribute(SymTabKeyImpl.ROUTINE_SYMTAB).asInstanceOf[SymTab]
    val newRecordTypes = new util.ArrayList[TypeSpec]()
    printSymTab(symTab, newRecordTypes)

    //Print cross-reference tables for any records defined in the routine
    if (newRecordTypes.size() > 0) {
      printRecords(newRecordTypes)
    }

    // Print any procedures and functions defined in the routine.
    val routineIds = routineId.getAttribute(SymTabKeyImpl.ROUTINE_ROUTINES).asInstanceOf[util.ArrayList[SymTabEntry]]
    if (routineIds != null) {
      for (rtnId <- routineIds.asScala) {
        printRoutine(rtnId)
      }
    }
  }

  /**
    * Print the headings for the 2 columns, "Identifier" and "Line numbers".
    */
  private def printColumnHeadings(): Unit = { // TODO: change the format if necessary.
    println()
    println(f"${"Identifier"}%-16s" + CrossReferencer.NUMBERS_LABEL + "Type specification")
    println(f"${"----------"}%-16s" + CrossReferencer.NUMBERS_UNDERLINE + "------------------")
  }

  /**
    * Print the symbol table.
    *
    * @param symTab symbol table.
    */
  private def printSymTab(symTab: SymTab, recordTypes: util.ArrayList[TypeSpec]): Unit = {
    val sorted = symTab.sortedEntries
    for (entry: SymTabEntry <- sorted.asScala) {
      val lineNumbers = entry.getLineNumbers

      // For each entry, print the identifier name
      // followed by the line numbers.
      Predef.print(f"${entry.getName}%-16s")
      if (lineNumbers != null) {
        for (lineNumber: Int <- lineNumbers.asScala) {
          Predef.print(f" $lineNumber%03d")
        }
      }

      // Print the symbol table entry.
      println()
      printEntry(entry, recordTypes)
    }
  }

  /**
    * Print an entry's info.
    *
    * @param entry       entry.
    * @param recordTypes record types.
    */
  private def printEntry(entry: SymTabEntry, recordTypes: util.ArrayList[TypeSpec]): Unit = {
    val definition = entry.getDefinition
    val nestingLevel = entry.getSymTab.getNestingLevel
    println(CrossReferencer.INDENT + "Defined as: " + definition.getText)
    println(CrossReferencer.INDENT + "Scope nesting level: " + nestingLevel)

    // Print the type specification.
    val typeSpec = entry.getTypeSpec
    printType(typeSpec)

    definition.asInstanceOf[DefinitionImpl] match {
      case DefinitionImpl.CONSTANT =>
        val value = entry.getAttribute(SymTabKeyImpl.CONSTANT_VALUE)
        println(CrossReferencer.INDENT + "Value = " + toString(value))

        // Print the type details only if the type is unnamed.
        if (typeSpec.getIdentifier == null) {
          printTypeDetail(typeSpec, recordTypes)
        }
      case DefinitionImpl.ENUMERATION_CONSTANT =>
        val value = entry.getAttribute(SymTabKeyImpl.CONSTANT_VALUE)
        println(CrossReferencer.INDENT + "Value = " + toString(value))

      case DefinitionImpl.TYPE =>
        // Print the type details only when the type is first defined.
        if (entry == typeSpec.getIdentifier) {
          printTypeDetail(typeSpec, recordTypes)
        }
      case DefinitionImpl.VARIABLE =>
        // Print the type details only if the type is unnamed.
        if (typeSpec.getIdentifier == null) {
          printTypeDetail(typeSpec, recordTypes)
        }
      case _ =>
    }
  }

  /**
    * Print a type specification.
    *
    * @param typeSpec the type specification.
    */
  private def printType(typeSpec: TypeSpec): Unit = {
    if (typeSpec != null) {
      val form = typeSpec.getForm
      val typeId = typeSpec.getIdentifier
      val typeName = if (typeId != null) typeId.getName else "<unnamed>"

      println(CrossReferencer.INDENT + "Type form = " + form + ", Type id = " + typeName)
    }
  }

  /**
    * Print the details about a type.
    *
    * @param typeSpec    type specification.
    * @param recordTypes record types.
    */
  private def printTypeDetail(typeSpec: TypeSpec, recordTypes: util.ArrayList[TypeSpec]): Unit = {
    val form = typeSpec.getForm

    form.asInstanceOf[TypeFormImpl] match {
      case TypeFormImpl.ENUMERATION =>
        val constantIds = typeSpec.getAttribute(TypeKeyImpl.ENUMERATION_CONSTANTS).asInstanceOf[util.List[SymTabEntry]]

        println(CrossReferencer.INDENT + "--- Enumeration constants ---")

        // Print each enumeration constant and its value.
        for (constantId: SymTabEntry <- constantIds.asScala) {
          val name = constantId.getName
          val value = constantId.getAttribute(SymTabKeyImpl.CONSTANT_VALUE)

          println(CrossReferencer.INDENT + f"$name%16s = $value%s")
        }
      case TypeFormImpl.SUBRANGE =>
        val minValue = typeSpec.getAttribute(TypeKeyImpl.SUBRANGE_MIN_VALUE)
        val maxValue = typeSpec.getAttribute(TypeKeyImpl.SUBRANGE_MAX_VALUE)
        val baseTypeSpec = typeSpec.getAttribute(TypeKeyImpl.SUBRANGE_BASE_TYPE).asInstanceOf[TypeSpec]

        println(CrossReferencer.INDENT + "--- Base type ---")

        // Print the base type details only if the type is unnamed.
        if (baseTypeSpec.getIdentifier == null) {
          printTypeDetail(baseTypeSpec, recordTypes)
        }

        scala.Predef.print(CrossReferencer.INDENT + "Range = ")
        println(toString(minValue) + ".." + toString(maxValue))

      case TypeFormImpl.ARRAY =>
        val indexType = typeSpec.getAttribute(TypeKeyImpl.ARRAY_INDEX_TYPE).asInstanceOf[TypeSpec]
        val elementType = typeSpec.getAttribute(TypeKeyImpl.ARRAY_ELEMENT_TYPE).asInstanceOf[TypeSpec]
        val count = typeSpec.getAttribute(TypeKeyImpl.ARRAY_ELEMENT_COUNT).asInstanceOf[Integer]

        println(CrossReferencer.INDENT + "--- INDEX TYPE ---")
        printType(indexType)

        // Print the index type details only if the type is unnamed.
        if (indexType.getIdentifier == null) {
          printTypeDetail(indexType, recordTypes)
        }

        println(CrossReferencer.INDENT + "--- ELEMENT TYPE ---")
        printType(elementType)
        println(CrossReferencer.INDENT.toString() + count + " elements")

        // Print the elements type details only if the type is unnamed.
        if (elementType.getIdentifier == null) {
          printTypeDetail(elementType, recordTypes)
        }
      case TypeFormImpl.RECORD =>
        recordTypes.add(typeSpec)
    }
  }

  /**
    * Print records.
    *
    * @param recordTypes record types.
    */
  private def printRecords(recordTypes: util.ArrayList[TypeSpec]): Unit = {
    for (recordType: TypeSpec <- recordTypes.asScala) {
      val recordId = recordType.getIdentifier
      val name = if (recordId != null) recordId.getName else "<unnamed>"

      println("\n-- RECORD " + name + " ---")
      printColumnHeadings()

      // Print the entries in the record's symbol table.
      val symTab = recordType.getAttribute(TypeKeyImpl.RECORD_SYMTAB).asInstanceOf[SymTab]
      val newRecordTypes = new util.ArrayList[TypeSpec]
      printSymTab(symTab, newRecordTypes)

      // Print cross-reference tables for any nested records.
      if (newRecordTypes.size() > 0) {
        printRecords(newRecordTypes)
      }
    }
  }

  /**
    * Convert a value to a string.
    *
    * @param value the value.
    * @return the string.
    */
  private def toString(value: Any): String = {
    if (value.isInstanceOf[String]) {
      "'" + value.asInstanceOf[String] + "'"
    } else {
      value.toString
    }
  }
}

/**
  * Some constants used by this class.
  */
object CrossReferencer {
  val NAME_WIDTH = 16
  val NUMBERS_LABEL = " Line numbers    "
  val NUMBERS_UNDERLINE = " ------------    "
  val LABEL_WIDTH = NUMBERS_LABEL.length
  val INDENT_WIDTH = NAME_WIDTH + LABEL_WIDTH
  val INDENT = new StringBuilder(INDENT_WIDTH)

  for (i <- 0 until INDENT_WIDTH) INDENT.append(" ")
}

