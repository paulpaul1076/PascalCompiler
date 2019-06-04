package backend.interpreter.executors

import java.util

import backend.interpreter.{ActivationRecord, Executor, MemoryFactory}
import intermediate.icodeimpl.ICodeKeyImpl
import intermediate.symtabimpl.{DefinitionImpl, SymTabKeyImpl}
import intermediate.{ICode, ICodeNode, SymTabEntry}

/**
  * Execute a call to a declared procedure or function.
  *
  * @param parent parent executor.
  */
class CallDeclaredExecutor(parent: Executor) extends CallExecutor(parent) {

  /**
    * Execute a call to a declared procedure or function.
    *
    * @param node the root node of the statement.
    * @return return value of this function or null if it's a procedure.
    */
  override def execute(node: ICodeNode): Any = {
    val routineId = node.getAttribute(ICodeKeyImpl.ID).asInstanceOf[SymTabEntry]
    val newAr = MemoryFactory.createActivationRecord(routineId)

    // Execute any actual parameters and initialize
    // the formal parameters in the new activation record.
    if (node.getChildren.size > 0) {
      val parmsNode = node.getChildren.get(0)
      val actualNodes = parmsNode.getChildren
      val formalIds = routineId.getAttribute(SymTabKeyImpl.ROUTINE_PARMS).asInstanceOf[util.ArrayList[SymTabEntry]]
      executeActualParms(actualNodes, formalIds, newAr)
    }

    // Push the new activation record.
    Executor.runtimeStack.push(newAr)

    sendCallMessage(node, routineId.getName)

    // Get the root node of the routine's intermediate code.
    val iCode = routineId.getAttribute(SymTabKeyImpl.ROUTINE_ICODE).asInstanceOf[ICode]
    val rootNode = iCode.getRoot

    // Execute the routine.
    val statementExecutor = new StatementExecutor(this)
    val value = statementExecutor.execute(rootNode)

    // Pop off the activation record.
    Executor.runtimeStack.pop()

    sendReturnMessage(node, routineId.getName)
    value
  }

  /**
    * Execute the actual parameters of a call.
    *
    * @param actualNodes the list of nodes of the actual parms.
    * @param formalIds   the list of symbol table entries of the formal parms.
    * @param newAr       the new activation record.
    */
  def executeActualParms(actualNodes: util.ArrayList[ICodeNode], formalIds: util.ArrayList[SymTabEntry], newAr: ActivationRecord): Unit = {
    val expressionExecutor = new ExpressionExecutor(this)
    val assignmentExecutor = new AssignmentExecutor(this)

    for (i <- 0 until formalIds.size()) {
      val formalId = formalIds.get(i)
      val formalDefn = formalId.getDefinition
      val formalCell = newAr.getCell(formalId.getName)
      val actualNode = actualNodes.get(i)

      // Value parameter.
      if (formalDefn == DefinitionImpl.VALUE_PARM) {
        val formalType = formalId.getTypeSpec
        val valueType = actualNode.getTypeSpec.baseType
        val value = expressionExecutor.execute(actualNode)

        assignmentExecutor.assignValue(actualNode, formalId, formalCell, formalType, value, valueType)
      }
      // Var parameter
      else {
        val actualCell = expressionExecutor.executeVariable(actualNode)
        formalCell.setValue(actualCell)
      }
    }
  }
}
