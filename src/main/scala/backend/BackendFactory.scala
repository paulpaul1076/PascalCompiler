package backend

import backend.compiler.CodeGenerator
import backend.interpreter.Executor

/**
 * A factory class that crates compiler and interpreter components.
 */
object BackendFactory {

  /**
   * Create a compiler or an interpreter back end component.
   *
   * @param operation either "compile" or "execute".
   * @return a compiler or an interpreter back end component.
   */
  def createBackend(operation: String): Backend = {
    if (operation.equalsIgnoreCase("compile")) {
      new CodeGenerator
    } else if (operation.equalsIgnoreCase("execute")) {
      new Executor(null)
    } else {
      throw new Exception("Backend factory: Invalid operation '" + operation + "'")
    }
  }
}
