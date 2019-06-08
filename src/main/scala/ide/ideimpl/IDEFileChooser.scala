package ide.ideimpl

import java.io._
import java.awt._
import javax.swing._
import javax.swing.filechooser.FileFilter


/**
  * <p>Title: </p>
  * <p>Description: </p>
  * <p>Copyright: Copyright (c) 2005</p>
  * <p>Company: NASA Ames Research Center</p>
  *
  * @author Ronald Mak
  * @version 2.0
  */
class IDEFileChooser private[ideimpl](val directoryPath: String, val filePath: String, val filter: IdeFileFilter, val directories: Boolean, val multiple: Boolean) extends JFileChooser {
  var varDirectoryPath = directoryPath
  this.setMultiSelectionEnabled(multiple)
  this.setFileSelectionMode(if (directories) JFileChooser.DIRECTORIES_ONLY
  else JFileChooser.FILES_ONLY)
  if (filePath != null) this.setSelectedFile(new File(filePath))
  else {
    if (directoryPath == null) varDirectoryPath = if (System.getProperties.getProperty("file.separator") == "/") System.getProperties.getProperty("user.home")
    else "c:\\"
    this.setCurrentDirectory(new File(varDirectoryPath))
  }
  if (filter != null) this.addChoosableFileFilter(filter)

  def this(directoryPath: String, filePath: String, filter: IdeFileFilter) {
    this(directoryPath, filePath, filter, false, false)
  }

  private[ideimpl] def choose(textField: JTextField, parent: Component) = {
    val option = this.showOpenDialog(parent)
    if (option == JFileChooser.APPROVE_OPTION) {
      val file = this.getSelectedFile
      textField.setText(file.getPath)
      file
    }
    else null
  }

  private[ideimpl] def choose(textField: JTextField, parent: Component, multiple: Boolean): File = {
    if (!multiple) return choose(textField, parent)
    val option = this.showOpenDialog(parent)
    if (option == JFileChooser.APPROVE_OPTION) {
      val files = this.getSelectedFiles
      val buffer = new StringBuffer
      var i = 0
      while ( {
        i < files.length
      }) {
        if (i > 0) buffer.append(";")
        buffer.append(files(i).getPath)

        {
          i += 1; i
        }
      }
      textField.setText(buffer.toString)
      files(0)
    }
    else null
  }
}

class IdeFileFilter(var ext: Array[String], var description: String) extends FileFilter {
  private var extensions: Array[String] = new Array[String](ext.length)

  var i = 0
  while ( {
    i < extensions.length
  }) {
    this.extensions(i) = ext(i).toLowerCase

    {
      i += 1; i
    }
  }

  override def accept(file: File): Boolean = {
    if (file.isDirectory) return true
    val name = file.getName.toLowerCase
    var i = 0
    while ( {
      i < extensions.length
    }) {
      if (name.endsWith(extensions(i))) return true

      {
        i += 1; i
      }
    }
    false
  }

  override def getDescription: String = description
}
