package backend.interpreter.executors

import backend.interpreter.Executor
import intermediate.ICodeNode
import intermediate.icodeimpl.ICodeNodeTypeImpl

import scala.collection.JavaConversions._
import scala.util.control.Breaks

/**
  * Execute a loop.
  */
class LoopExecutor(parent: Executor) extends StatementExecutor(parent) {
  /**
    * Execute a loop statement.
    *
    * @param node the root node of the statement.
    * @return null.
    */
  override def execute(node: ICodeNode): Any = {
    var exitLoop = false
    var exprNode: ICodeNode = null
    val loopChildren = node.getChildren

    val expressionExecutor = new ExpressionExecutor(this)
    val statementExecutor = new StatementExecutor(this)

    // Loop until the TEST expression value is true.
    while (!exitLoop) {
      Executor.executionCount += 1 // count the loop statement itself

      val loop = new Breaks
      // Execute the children of this LOOP node.
      loop.breakable {
        for (child: ICodeNode <- loopChildren) {
          val childType = child.getType.asInstanceOf[ICodeNodeTypeImpl]

          // TEST node?
          if (childType == ICodeNodeTypeImpl.TEST) {
            if (exprNode == null) {
              exprNode = child.getChildren.get(0)
            }
            exitLoop = expressionExecutor.execute(exprNode).asInstanceOf[Boolean]
          }
          // Statement node.
          else {
            statementExecutor.execute(child)
          }
          // Exit if the TEST expression value is true.
          if (exitLoop) {
            loop.break
          }
        }
      }
    }
    null
  }
}
