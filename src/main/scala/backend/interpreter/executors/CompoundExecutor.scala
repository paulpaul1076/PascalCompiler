package backend.interpreter.executors

import backend.interpreter.Executor
import intermediate.ICodeNode

/**
 * Executor for compound statements.
 *
 * @param parent parent executor.
 */
class CompoundExecutor(parent: Executor) extends StatementExecutor(parent) {
  /**
   * Execute a compound statement.
   *
   * @param node the root node of the statement.
   * @return null.
   */
  override def execute(node: ICodeNode): Any = {
    // Loop over the children of the COMPOUND node and execute each child.
    val statementExecutor = new StatementExecutor(this)
    val children = node.getChildren
    children.stream().forEach(child => statementExecutor.execute(child))
    null
  }
}
