package backend

import java.lang

import backend.compiler.CodeGenerator
import backend.interpreter.Executor
import intermediate.TypeSpec
import intermediate.symtabimpl.Predefined

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

  def defaultValue(`type`: TypeSpec): Any = {
    val type1 = `type`.baseType

    if (type1 == Predefined.integerType) {
      new Integer(0)
    } else if (type1 == Predefined.realType) {
      new lang.Float(0.0f)
    } else if (type1 == Predefined.booleanType) {
      new lang.Boolean(false)
    } else if (type1 == Predefined.charType) {
      new lang.Character('#')
    } else /*string*/ {
      new String("#")
    }
  }
}
