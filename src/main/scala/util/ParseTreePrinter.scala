package util

import java.io.PrintStream
import java.util

import intermediate.icodeimpl.ICodeNodeImpl
import intermediate.symtabimpl.SymTabKeyImpl
import intermediate.{ICode, ICodeNode, SymTabEntry, SymTabStack}

import scala.collection.JavaConverters._

/**
  * ParseTreePrinter.
  */
class ParseTreePrinter(val ps: PrintStream) {
  private var length: Int = 0
  private var indent: String = ""
  private var indentation: String = ""
  private val line: StringBuilder = new StringBuilder

  /**
    * Constructor stuff.
    */
  for (i <- 0 until ParseTreePrinter.INDENT_WIDTH) indent += " "

  /**
    * Print the AST.
    *
    * @param iCode AST object.
    */
  def print(iCode: ICode): Unit = {
    ps.println("\n===== INTERMEDIATE CODE =====\n")
    if (iCode.getRoot != null) { // had a problem without the if.
      printNode(iCode.getRoot.asInstanceOf[ICodeNodeImpl])
      printLine()
    }
  }

  /**
    * Print a node of the AST.
    *
    * @param node current node.
    */
  def printNode(node: ICodeNodeImpl): Unit = {
    // Opening tag.
    append(indentation)
    append("<" + node.toString)

    printAttributes(node)
    printTypeSpec(node)

    val childNodes = node.getChildren

    // Print the node's children followed by the closing tag.
    if ((childNodes != null) && (childNodes.size() > 0)) {
      append(">")
      printLine()

      printChildNodes(childNodes)
      append(indentation)
      append("</" + node.toString + ">")
    } else {
      append(" ")
      append("/>")
    }

    printLine()
  }

  /**
    * Append text to the output line.
    *
    * @param text the text to append.
    */
  def append(text: String): Unit = {
    val textLength = text.length
    var lineBreak = false

    // Wrap lines that are too long.
    if (length + textLength > ParseTreePrinter.LINE_WIDTH) {
      printLine()
      line.append(indentation)
      length = indentation.length
      lineBreak = true
    }

    // Append the text.
    if (!(lineBreak && text.equals(" "))) {
      line.append(text)
      length += textLength
    }
  }

  /**
    * Print an output line.
    */
  def printLine(): Unit = {
    if (length > 0) {
      ps.println(line)
      line.setLength(0)
      length = 0
    }
  }

  /**
    * Print the attributes of this node.
    *
    * @param node current node.
    */
  private def printAttributes(node: ICodeNodeImpl): Unit = {
    val saveIndentation = indentation
    indentation += indent

    node.entrySet().stream().forEach(entry => {
      printAttribute(entry.getKey.toString, entry.getValue)
    })

    indentation = saveIndentation
  }

  /**
    * Print the intermediate code as a parse tree.
    *
    * @param symTabStack the symbol table stack.
    */
  def print(symTabStack: SymTabStack): Unit = {
    ps.println("\n===== INTERMEDIATE CODE =====")

    val programId = symTabStack.getProgramId()
    printRoutine(programId)
  }

  private def printTypeSpec(impl: ICodeNodeImpl): Unit = {

  }

  /**
    * Print the child nodes of this node.
    *
    * @param nodes the list of child nodes.
    */
  private def printChildNodes(nodes: util.ArrayList[ICodeNode]): Unit = {
    val saveIndentation = indentation
    indentation += indent

    nodes.stream().forEach(child => {
      printNode(child.asInstanceOf[ICodeNodeImpl])
    })

    indentation = saveIndentation
  }

  /**
    * Print attribute.
    *
    * @param keyString key as string.
    * @param value     value.
    */
  private def printAttribute(keyString: String, value: Any): Unit = {
    val isSymTabEntry = value.isInstanceOf[SymTabEntry]
    val valueString = if (isSymTabEntry) value.asInstanceOf[SymTabEntry].getName else value.toString

    val text = keyString.toLowerCase + "=\"" + valueString + "\""
    append(" ")
    append(text)

    // Include an identifier's nesting level.
    if (isSymTabEntry) {
      val level = value.asInstanceOf[SymTabEntry].getSymTab.getNestingLevel
      printAttribute("LEVEL", level)
    }
  }

  private def printRoutine(routineId: SymTabEntry): Unit = {
    val definition = routineId.getDefinition
    println(s"\n*** ${definition.toString} ${routineId.getName} ***\n")

    // Print the intermediate code in the routine's symbol table entry.
    val iCode = routineId.getAttribute(SymTabKeyImpl.ROUTINE_ICODE).asInstanceOf[ICode]
    if (iCode.getRoot != null) {
      printNode(iCode.getRoot.asInstanceOf[ICodeNodeImpl])
    }

    // Print any procedures and functions defined in the routine.
    val routineIds = routineId.getAttribute(SymTabKeyImpl.ROUTINE_ROUTINES).asInstanceOf[util.ArrayList[SymTabEntry]]
    if (routineIds != null) {
      for (rtnId: SymTabEntry <- routineIds.asScala) {
        printRoutine(rtnId)
      }
    }
  }

  /**
    * This is for constants.
    */
  private object ParseTreePrinter {
    /**
      * One tab width.
      */
    val INDENT_WIDTH = 4

    /**
      * Maximum line width.
      */
    val LINE_WIDTH = 80
  }

}