package backend.interpreter.executors

import backend.interpreter.Executor
import intermediate.ICodeNode

/**
  * Execute an IF statement.
  *
  * @param parent parent executor.
  */
class IfExecutor(parent: Executor) extends StatementExecutor(parent) {

  /**
    * Execute an IF statement.
    *
    * @param node the root node of the statement.
    * @return null.
    */
  override def execute(node: ICodeNode): Any = {
    // Get the IF node's children.
    val children = node.getChildren
    val exprNode = children.get(0)
    val thenStmtNode = children.get(1)
    val elseStmtNode = if (children.size() > 2) children.get(2) else null

    val expressionExecutor = new ExpressionExecutor(this)
    val statementExecutor = new StatementExecutor(this)

    // Evaluate the expression to determine which statement to execute.
    val b = expressionExecutor.execute(exprNode).asInstanceOf[Boolean]
    if (b) {
      statementExecutor.execute(thenStmtNode)
    } else if (thenStmtNode != null) {
      statementExecutor.execute(elseStmtNode)
    }

    Executor.executionCount += 1 // count the IF statement itself
    null
  }
}
