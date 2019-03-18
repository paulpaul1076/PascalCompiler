package util

import intermediate.{SymTab, SymTabEntry, SymTabStack}

/**
 * Generate a cross-reference listing.
 */
class CrossReferencer {

  /**
   * Print the table tag.
   *
   * @param symTabStack the symol table stack.
   */
  def print(symTabStack: SymTabStack): Unit = {
    println("\n===== CROSS-REFERENCE TABLE =====")
    printColumnHeadings()

    printSymTab(symTabStack.getLocalSymTab)
  }

  /**
   * Print the headings for the 2 columns, "Identifier" and "Line numbers".
   */
  private def printColumnHeadings(): Unit = {
    println()
    println(f"${"Identifier"}%-16s" + CrossReferencer.NUMBERS_LABEL)
    println(f"${"----------"}%-16s" + CrossReferencer.NUMBERS_UNDERLINE)
  }

  /**
   * Print the symbol table.
   *
   * @param symTab symbol table.
   */
  private def printSymTab(symTab: SymTab): Unit = {
    /*for (entry: SymTabEntry <- symTab.sortedEntries)*/ symTab.sortedEntries.forEach(entry => {
      val lineNumbers = entry.getLineNumbers

      scala.Predef.print(f"${entry.getName}%-16s ")
      if (lineNumbers != null) {
        /*for (lineNumber: Int <- lineNumbers)*/ lineNumbers.stream().forEach(lineNumber => {
          scala.Predef.print(f"$lineNumber%03d ")
        })
      }
      println()
    })
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

}