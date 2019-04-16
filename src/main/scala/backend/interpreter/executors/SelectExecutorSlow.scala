package backend.interpreter.executors

import java.util

import backend.interpreter.Executor
import intermediate.ICodeNode
import intermediate.icodeimpl.ICodeKeyImpl

import scala.collection.JavaConversions._

/**
  * Select statement executor class.
  *
  * @param parent parent executor.
  */
class SelectExecutorSlow(parent: Executor) extends StatementExecutor(parent) {

  /**
    * Execute SELECT statement.
    *
    * @param node the root node of the statement.
    * @return null.
    */
  override def execute(node: ICodeNode): Any = {
    // Get the SELECT node's children.
    val selectChildren = node.getChildren
    val exprNode = selectChildren.get(0)

    // Evaluate the SELECT expression.
    val expressionExecutor = new ExpressionExecutor(this)
    val selectValue = expressionExecutor.execute(exprNode)

    // Attempt to select a SELECT_BRANCH.
    val selectedBranchNode = searchBranches(selectValue, selectChildren)

    // If there was a selection, execute the SELECT_BRANCH's statement.
    if (selectedBranchNode != null) {
      val stmtNode = selectedBranchNode.getChildren.get(1)
      val statementExecutor = new StatementExecutor(this)
      statementExecutor.execute(stmtNode)
    }

    Executor.executionCount += 1 // count the SELECT statement itself
    null
  }

  /**
    * Search the SELECT_BRANCHes to find a match.
    *
    * @param selectValue    the value to match.
    * @param selectChildren the children of the Select node.
    * @return ICodeNode.
    */
  private def searchBranches(selectValue: Any, selectChildren: util.ArrayList[ICodeNode]): ICodeNode = {
    // Loop over the SELECT_BRANCHes to find a match.
    for (i <- 1 until selectChildren.size()) {
      if (searchConstants(selectValue, selectChildren.get(i))) {
        return selectChildren.get(i)
      }
    }
    null
  }

  /**
    * Search the constants of a SELECT_BRANCH for a matching valvue.
    *
    * @param value      value to search for.
    * @param branchNode the SELECT_BRANCH node.
    * @return boolean.
    */
  private def searchConstants(value: Any, branchNode: ICodeNode): Boolean = {
    // Get the list of SELECT_CONSTANTS values.
    val constantsNode = branchNode.getChildren.get(0)
    val constantsList = constantsNode.getChildren

    // Search the list of constants.
    if (value.isInstanceOf[Int]) {
      for (constantsNode: ICodeNode <- constantsList) {
        val constant = constantsNode.getAttribute(ICodeKeyImpl.VALUE)
        if (value.asInstanceOf[Int].equals(constant)) {
          return true
        }
      }
    } else {
      for (constantsNode: ICodeNode <- constantsList) {
        val constant = constantsNode.getAttribute(ICodeKeyImpl.VALUE).asInstanceOf[String]
        if (value.asInstanceOf[String].equals(constant)) {
          return true
        }
      }
    }
    false
  }
}
