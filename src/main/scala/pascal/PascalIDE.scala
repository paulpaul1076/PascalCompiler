import java.awt._
import java.awt.event._

import javax.swing._
import javax.swing.border._
import javax.swing.UIManager
import ide._
import ide.ideimpl.IDEFrame


/**
  * <h1>PascalIDE</h1>
  *
  * <p>The simple Pascal Integrated Development Environment.</p>
  *
  * <p>Copyright (c) 2009 by Ronald Mak</p>
  * <p>For instructional purposes only.  No warranties.</p>
  */
object PascalIDE {
  def main(args: Array[String]): Unit = {
    new PascalIDE
  }
}

class PascalIDE() {
  new IDEFrame
}
