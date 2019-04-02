package backend.interpreter.executors

import java.util

import backend.interpreter.Executor
import intermediate.ICodeNode
import intermediate.icodeimpl.ICodeKeyImpl

import scala.collection.JavaConversions._

/**
  * A faster version of SelectExecutor slow, making use of jump tables.
  *
  * @param parent parent executor.
  */
class SelectExecutorFast(parent: Executor) extends StatementExecutor(parent) {

  /**
    * Execute this select node.
    *
    * @param node the root node of the statement.
    * @return null.
    */
  override def execute(node: ICodeNode): Any = {
    // Is there already an entry for this SELECT node in the jump table cache?
    // If not, create a jump table entry.
    var jumpTable = SelectExecutorFast.jumpCache.get(node)
    if (jumpTable == null) {
      jumpTable = createJumpTable(node)
      SelectExecutorFast.jumpCache.put(node, jumpTable)
    }

    // Get the SELECT node's expression child.
    val exprNode = node.getChildren.get(0)

    // Execute the expression.
    val expressionExecutor = new ExpressionExecutor(this)
    val selectValue = expressionExecutor.execute(exprNode)

    // If there is a selection, execute the SELECT_BRANCH's statement.
    val statementNode = jumpTable.get(selectValue)
    if (statementNode != null) {
      val statementExecutor = new StatementExecutor(this)
      statementExecutor.execute(statementNode)
    }

    Executor.executionCount += 1 // Count the select statement.
    null
  }

  /**
    * Create a jump table for this particular SELECT node.
    *
    * @param selectNode the select node.
    * @return the jump table.
    */
  private def createJumpTable(selectNode: ICodeNode): util.HashMap[Any, ICodeNode] = {
    val jumpTable = new util.HashMap[Any, ICodeNode]()

    // Loop over children that are SELECT_BRANCH nodes.
    val selectChildren = selectNode.getChildren
    for (i <- 1 until selectChildren.size()) {
      val child = selectChildren.get(i)
      val constants = child.getChildren.get(0).getChildren
      for (value: ICodeNode <- constants) {
        jumpTable.put(value.getAttribute(ICodeKeyImpl.VALUE), child.getChildren.get(1))
      }
    }
    jumpTable
  }
}

/**
  * Companion object.
  */
object SelectExecutorFast {
  // Jump table cache: entry key is a SELECT node,
  //                   entry value is the jump table.
  // Jump table: entry key is a selection value,
  //             entry value is the branch statement.
  val jumpCache = new util.HashMap[ICodeNode, util.HashMap[Any, ICodeNode]]()
}
