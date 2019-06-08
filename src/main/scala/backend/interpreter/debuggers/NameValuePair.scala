package backend.interpreter.debuggers

import java.util

import backend.interpreter.Cell

import scala.util.control.Breaks

/**
  * Class for prettifying different pascal objects.
  *
  * @param variableNamee variable name.
  * @param value         value of the variable.
  */
class NameValuePair(variableNamee: String, value: Any) {
  private var variableName: String = variableNamee // variable's name
  private var valueString: String = NameValuePair.valueString(value) // variable's value name

  def getVariableName: String = variableName

  def getValueString: String = valueString

}

/**
  * Companion object.
  */
object NameValuePair {
  val MAX_DISPLAYED_ELEMENTS = 10

  def valueString(value: Any): String = {
    val buffer = new StringBuilder

    // Undefined value.
    if (value == null) {
      buffer.append("?")
    }

    // Defederence a VAR parameter
    else if (value.isInstanceOf[Cell]) {
      buffer.append(valueString(value.asInstanceOf[Cell].getValue))
    }

    // Array value
    else if (value.isInstanceOf[Array[Cell]]) {
      arrayValueStrig(value.asInstanceOf[Array[Cell]], buffer)
    }

    // Record value.
    else if (value.isInstanceOf[util.HashMap[String, Cell]]) {
      recordValueString(value.asInstanceOf[util.HashMap[String, Cell]], buffer)
    }

    // Character value.
    else if (value.isInstanceOf[Character]) {
      buffer.append("'").append(value.asInstanceOf[Character]).append("'")
    }

    // Numeric or boolean value.
    else {
      buffer.append(value.toString)
    }

    return buffer.toString()
  }

  def arrayValueStrig(array: Array[Cell], buffer: StringBuilder): Unit = {
    var elementCount = 0
    var first = true
    buffer.append("[")

    // Loop over each array element up to MAX_DISPLAYED_ELEMENTS times.
    val loop = new Breaks

    loop.breakable {
      for (cell <- array) {
        if (first) {
          first = false
        } else {
          buffer.append(", ")
        }

        elementCount += 1
        if (elementCount <= MAX_DISPLAYED_ELEMENTS) {
          buffer.append(cell.getValue)
        } else {
          buffer.append("...")
          loop.break()
        }
      }
    }

    buffer.append("]")
  }

  def recordValueString(record: util.HashMap[String, Cell], buffer: StringBuilder): Unit = {
    var first = true
    buffer.append("{")

    val entries = record.entrySet()
    val it = entries.iterator()

    // Loop over each record field.
    while (it.hasNext) {
      val entry = it.next()

      if (first) {
        first = false
      } else {
        buffer.append(", ")
      }

      buffer.append(entry.getKey).append(": ").append(valueString(entry.getValue.getValue))
    }

    buffer.append("}")
  }
}
