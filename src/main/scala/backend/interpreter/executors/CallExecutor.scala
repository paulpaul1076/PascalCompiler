package backend.interpreter.executors

import backend.interpreter.Executor
import intermediate.icodeimpl.ICodeKeyImpl
import intermediate.symtabimpl.{RoutineCodeImpl, SymTabKeyImpl}
import intermediate.{ICodeNode, RoutineCode, SymTabEntry}

/**
  * Execute a call to a procedure or function.
  * @param parent parent executor.
  */
class CallExecutor(parent: Executor) extends StatementExecutor(parent) {

  /**
    * Execute procedure or function call statement.
    * @param node the root node of the statement.
    * @return null.
    */
  override def execute(node: ICodeNode): Any = {
    val routineId = node.getAttribute(ICodeKeyImpl.ID).asInstanceOf[SymTabEntry]
    val routineCode = routineId.getAttribute(SymTabKeyImpl.ROUTINE_CODE).asInstanceOf[RoutineCode]
    val callExecutor =
      if (routineCode == RoutineCodeImpl.DECLARED)
        new CallDeclaredExecutor(this)
      else
        new CallStandardExecutor(this)

    Executor.executionCount += 1
    callExecutor.execute(node)
  }
}
