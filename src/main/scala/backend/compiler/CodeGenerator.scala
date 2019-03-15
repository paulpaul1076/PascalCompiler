package backend.compiler

import backend.Backend
import intermediate.{ICode, SymTab}
import message.{Message, MessageListener, MessageType}

/**
  * Compiler backend.
  */
class CodeGenerator extends Backend {
  /**
   * Process the intermediate code and the symbol table generated by the parser.
   * To be implemented by a compiler or an interpreter subclass.
   *
   * @param iCode  the intermediate code.
   * @param symTab the symbol table.
   */
  override def process(iCode: ICode, symTab: SymTab): Unit = {
    val startTime = System.currentTimeMillis()
    val elapsedTime = (System.currentTimeMillis() - startTime) / 1000f
    val instructionCount = 0

    sendMessage(new Message(MessageType.COMPILER_SUMMARY, List[Any](instructionCount, elapsedTime)))
  }
}