package backend.compiler

class Label {
  Label.index += 1
  private var label: String = "L" + String.format("%03d", Label.index)
  override def toString: String = label
}

object Label {
  var index = 0
}
