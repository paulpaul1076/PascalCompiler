package ide.ideimpl

import java.io._
import java.util._
import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.border._
import javax.swing.event._
import java.awt.Dimension
import java.awt.BorderLayout
import ide.IDEControl


/**
  * <h1>EditFrame</h1>
  *
  * <p>The edit window of the Pascal IDE.</p>
  *
  * <p>Copyright (c) 2009 by Ronald Mak</p>
  * <p>For instructional purposes only.  No warranties.</p>
  */
class EditFrame() extends JInternalFrame with ActionListener with CaretListener {


  private var parserMessagesBorder: TitledBorder = _
  private var syntaxMessagesBorder: TitledBorder = _
  private val contentBorderLayout = new BorderLayout
  private val parserMessagesBorderLayout = new BorderLayout
  private val controlGridBagLayout = new GridBagLayout
  private val positionGridBagLayout = new GridBagLayout
  private val positionPanel = new JPanel
  private val parserPanel = new JPanel
  private val controlPanel = new JPanel
  private val splitPane = new JSplitPane
  private val sourceScrollPane = new JScrollPane
  private val syntaxScrollPane = new JScrollPane
  private val sourceArea = new JTextArea
  private val parserTextArea = new JTextArea
  private val sourceFileLabel = new JLabel
  private val sourcePathText = new JTextField
  private val browseSourceButton = new JButton
  private val saveSourceButton = new JButton
  private val inputFileLabel = new JLabel
  private val inputPathText = new JTextField
  private val browseInputButton = new JButton
  private val clearInputButton = new JButton
  private val lineLabel = new JLabel
  private val lineText = new JTextField
  private val columnLabel = new JLabel
  private val columnText = new JTextField
  private val runProgramButton = new JButton
  private val syntaxList = new JList[Any]
  private var control: IDEControl = _
  private var sourceName: String = _
  private var syntaxErrors: Vector[Any] = _

  /**
    * Constructor.
    */

  try
    initGuiComponents()
  catch {
    case ex: Exception =>
      ex.printStackTrace()
  }

  /**
    * Constructor.
    *
    * @param control the IDE control.
    */
  def this(control: IDEControl) {
    this
    this.control = control
    sourceArea.addCaretListener(this)
    browseSourceButton.addActionListener(this)
    saveSourceButton.addActionListener(this)
    browseInputButton.addActionListener(this)
    clearInputButton.addActionListener(this)
    runProgramButton.addActionListener(this)
    // Mouse click listener for the syntax errors.
    syntaxList.addMouseListener(new MouseAdapter() {
      override def mouseClicked(event: MouseEvent): Unit = {
        highlightError(syntaxList.locationToIndex(event.getPoint))
      }
    })
  }

  /**
    * Initialize the GUI components.
    *
    * @throws Exception if an error occurred.
    */
  @throws[Exception]
  private def initGuiComponents(): Unit = {
    parserMessagesBorder = new TitledBorder(BorderFactory.createLineBorder(new Color(153, 153, 153), 2), "Parser messages")
    syntaxMessagesBorder = new TitledBorder(BorderFactory.createLineBorder(new Color(153, 153, 153), 2), "Syntax error messages (click message to select source line)")
    this.setIconifiable(true)
    this.setMaximizable(true)
    this.setResizable(true)
    this.getContentPane.setLayout(contentBorderLayout)
    this.getContentPane.add(splitPane, BorderLayout.CENTER)
    splitPane.setOrientation(JSplitPane.VERTICAL_SPLIT)
    splitPane.setDividerLocation(300)
    splitPane.add(sourceScrollPane, JSplitPane.TOP)
    splitPane.add(controlPanel, JSplitPane.BOTTOM)
    sourceScrollPane.getViewport.add(sourceArea, null)
    sourceArea.setFont(new Font("Courier", 0, 12))
    controlPanel.setLayout(controlGridBagLayout)
    controlPanel.setBorder(BorderFactory.createLoweredBevelBorder)
    sourceFileLabel.setText("Source file:")
    sourcePathText.setEditable(false)
    browseSourceButton.setSelected(false)
    browseSourceButton.setText("Browse ...")
    saveSourceButton.setEnabled(false)
    saveSourceButton.setText("Save file")
    inputFileLabel.setText("Input file:")
    inputPathText.setEditable(false)
    browseInputButton.setText("Browse ...")
    clearInputButton.setEnabled(false)
    clearInputButton.setText("Clear")
    positionPanel.setLayout(positionGridBagLayout)
    lineLabel.setText("Line:")
    lineText.setMinimumSize(new Dimension(35, 20))
    lineText.setPreferredSize(new Dimension(35, 20))
    lineText.setEditable(false)
    columnLabel.setText("Column:")
    columnText.setMinimumSize(new Dimension(20, 20))
    columnText.setPreferredSize(new Dimension(20, 20))
    columnText.setEditable(false)
    columnText.setText("")
    runProgramButton.setEnabled(false)
    runProgramButton.setText("Run program")
    parserPanel.setLayout(parserMessagesBorderLayout)
    parserPanel.setBorder(parserMessagesBorder)
    parserPanel.setMinimumSize(new Dimension(12, 50))
    parserPanel.setPreferredSize(new Dimension(12, 50))
    parserPanel.add(parserTextArea, BorderLayout.CENTER)
    parserTextArea.setFont(new Font("Dialog", 1, 12))
    parserTextArea.setForeground(Color.blue)
    parserTextArea.setEditable(false)
    parserTextArea.setText(" ")
    syntaxScrollPane.setBorder(syntaxMessagesBorder)
    syntaxScrollPane.setMinimumSize(new Dimension(33, 90))
    syntaxScrollPane.setPreferredSize(new Dimension(60, 90))
    syntaxScrollPane.getViewport.add(syntaxList, null)
    syntaxList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    controlPanel.add(sourceFileLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(5, 5, 0, 0), 0, 0))
    controlPanel.add(sourcePathText, new GridBagConstraints(1, 0, 2, 1, 1.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 0, 0), 0, 0))
    controlPanel.add(browseSourceButton, new GridBagConstraints(3, 0, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 0, 0), 0, 0))
    controlPanel.add(saveSourceButton, new GridBagConstraints(4, 0, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 0, 5), 0, 0))
    controlPanel.add(inputFileLabel, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(5, 5, 0, 0), 0, 0))
    controlPanel.add(inputPathText, new GridBagConstraints(1, 1, 2, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 0, 0), 0, 0))
    controlPanel.add(browseInputButton, new GridBagConstraints(3, 1, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 0, 0), 0, 0))
    controlPanel.add(clearInputButton, new GridBagConstraints(4, 1, 1, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 0, 5), 0, 0))
    controlPanel.add(positionPanel, new GridBagConstraints(0, 2, 2, 1, 0.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.NONE, new Insets(5, 5, 0, 0), 0, 0))
    controlPanel.add(runProgramButton, new GridBagConstraints(3, 2, 2, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(5, 5, 0, 5), 0, 0))
    controlPanel.add(parserPanel, new GridBagConstraints(0, 3, 5, 1, 1.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 0, 5), 0, 0))
    controlPanel.add(syntaxScrollPane, new GridBagConstraints(0, 4, 5, 1, 1.0, 1.0, GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(0, 5, 5, 5), 0, 0))
    positionPanel.add(lineLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 0, 0, 0), 0, 0))
    positionPanel.add(lineText, new GridBagConstraints(1, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 5, 0, 0), 0, 0))
    positionPanel.add(columnLabel, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 15, 0, 0), 0, 0))
    positionPanel.add(columnText, new GridBagConstraints(3, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(0, 5, 0, 0), 0, 0))
  }

  /**
    * Position the caret in the source area.
    *
    * @param event the caret event.
    */
  override def caretUpdate(event: CaretEvent): Unit = {
    val dot = event.getDot
    try {
      val line = sourceArea.getLineOfOffset(dot)
      lineText.setText(Integer.toString(line + 1))
      val column = dot - sourceArea.getLineStartOffset(line)
      columnText.setText(Integer.toString(column + 1))
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }
  }

  /**
    * Button event dispatcher.
    *
    * @param event the button event.
    */
  override def actionPerformed(event: ActionEvent): Unit = {
    val button = event.getSource
    if (button eq browseSourceButton) browseSourceAction()
    else if (button eq saveSourceButton) saveSourceAction()
    else if (button eq browseInputButton) browseInputAction()
    else if (button eq clearInputButton) clearInputAction()
    else if (button eq runProgramButton) runProgramAction()
  }

  /**
    * Browse source button event handler.
    */
  private def browseSourceAction(): Unit = {
    val chooser = new IDEFileChooser(System.getProperty("user.dir"), null, new IdeFileFilter(Array[String](".pas"), "Pascal files (*.pas)"))
    val file = chooser.choose(sourcePathText, this)
    if (file != null) {
      sourceName = file.getName
      control.setSourcePath(file.getPath)
      setTitle("EDIT: " + sourceName)
      sourceArea.setText(null)
      var sourceFile: BufferedReader = null
      try {
        sourceFile = new BufferedReader(new FileReader(file))
        var line: String = sourceFile.readLine()
        while ( {
          line != null
        })  {
          sourceArea.append(line + "\n")
          line = sourceFile.readLine()
        }
        sourceArea.setCaretPosition(0)
        runProgramButton.setEnabled(true)
        saveSourceButton.setEnabled(true)

      } catch {
        case ex: Exception =>
          ex.printStackTrace()
      } finally try
        sourceFile.close()
      catch {
        case ignore: Exception =>

      }
    }
  }

  /**
    * Save source button event handler.
    */
  private def saveSourceAction(): Unit = {
    var sourceFile: BufferedWriter = null
    try {
      sourceFile = new BufferedWriter(new FileWriter(control.getSourcePath, false))
      sourceFile.write(sourceArea.getText)
    } catch {
      case ex: IOException =>
        ex.printStackTrace()
    } finally if (sourceFile != null) try
      sourceFile.close()
    catch {
      case ignore: Exception =>

    }
  }

  /**
    * Browse input button event handler.
    */
  private def browseInputAction(): Unit = {
    val chooser = new IDEFileChooser(System.getProperty("user.dir"), null, new IdeFileFilter(Array[String](".in", "*.txt", "*.dat"), "Input files (*.in, *.txt, *.dat)"))
    val file = chooser.choose(inputPathText, this)
    if (file != null) {
      control.setInputPath(file.getPath)
      clearInputButton.setEnabled(true)
    }
  }

  /**
    * Clear input button event handler.
    */
  private def clearInputAction(): Unit = {
    inputPathText.setText("")
    clearInputButton.setEnabled(false)
    control.setInputPath(null)
  }

  /**
    * Run program button event handler.
    */
  private def runProgramAction(): Unit = {
    syntaxErrors = new Vector[Any]
    saveSourceAction()
    setMessage("", Color.BLUE)
    control.clearEditWindowErrors
    control.clearDebugWindowListing
    control.clearConsoleWindowOutput
    control.stopDebuggerProcess
    control.startDebuggerProcess(sourceName)
  }

  /**
    * Set the editor message.
    *
    * @param message the message.
    * @param color   the message color.
    */
  def setMessage(message: String, color: Color): Unit = {
    parserTextArea.setForeground(color)
    parserTextArea.setText(message)
  }

  /**
    * Clear the syntax errors.
    */
  def clearEditWindowErrors(): Unit = {
    syntaxErrors.clear()
    syntaxList.setListData(syntaxErrors)
  }

  /**
    * Add a syntax error message.
    *
    * @param line the error message.
    */
  def addError(line: String): Unit = {
    syntaxErrors.addElement(line)
    syntaxList.setListData(syntaxErrors)
  }

  /**
    * Highlight a source line containing a syntax error.
    *
    * @param index the syntax error message index.
    */
  private def highlightError(index: Int): Unit = {
    val message = syntaxErrors.elementAt(index).asInstanceOf[String]
    val i = message.indexOf(":")
    // Extract the source line number from the syntax error message.
    if ((i != -1) && (i < 10)) {
      val lineNumber = message.substring(0, i).trim.toInt - 1
      // Highlight the source line.
      try {
        val start = sourceArea.getLineStartOffset(lineNumber)
        val end = sourceArea.getLineEndOffset(lineNumber)
        sourceArea.requestFocus()
        sourceArea.setSelectionStart(start)
        sourceArea.setSelectionEnd(end - 1)
      } catch {
        case ex: Exception =>
          ex.printStackTrace()
      }
    }
  }
}
