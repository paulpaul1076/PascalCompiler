package ide.ideimpl

import java.awt._
import java.awt.event._
import javax.swing._
import ide.IDEControl


/**
  * <h1>ConsoleFrame</h1>
  *
  * <p>The console window of the Pascal IDE.</p>
  *
  * <p>Copyright (c) 2009 by Ronald Mak</p>
  * <p>For instructional purposes only.  No warranties.</p>
  */
class ConsoleFrame() extends JInternalFrame with ActionListener {


  private val contentBorderLayout = new BorderLayout
  private val inputGridBagLayout = new GridBagLayout
  private val scrollPane = new JScrollPane
  private val outputArea = new JTextArea
  private val inputPanel = new JPanel
  private val inputLabel = new JLabel
  private val inputText = new JTextField
  private val enterButton = new JButton
  private var control: IDEControl = _

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
    enterButton.addActionListener(this)
  }

  /**
    * Initialize the GUI components.
    *
    * @throws Exception if an error occurred.
    */
  @throws[Exception]
  private def initGuiComponents(): Unit = {
    this.setIconifiable(true)
    this.setMaximizable(true)
    this.setResizable(true)
    this.getContentPane.setLayout(contentBorderLayout)
    this.getContentPane.add(scrollPane, BorderLayout.CENTER)
    this.getContentPane.add(inputPanel, java.awt.BorderLayout.SOUTH)
    inputPanel.setLayout(inputGridBagLayout)
    inputLabel.setText("Runtime input text:")
    enterButton.setText("Enter")
    scrollPane.getViewport.add(outputArea, null)
    outputArea.setFont(new Font("Courier", Font.PLAIN, 14))
    outputArea.setDoubleBuffered(true)
    outputArea.setEditable(false)
    inputPanel.add(inputLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(5, 5, 5, 0), 0, 0))
    inputPanel.add(inputText, new GridBagConstraints(1, 0, 1, 1, 1.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 5, 0), 0, 0))
    inputPanel.add(enterButton, new GridBagConstraints(2, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.NONE, new Insets(5, 5, 5, 5), 0, 0))
  }

  /**
    * Initialize the console window.
    */
  def initialize(): Unit = {
    inputText.setEnabled(true)
    enterButton.setEnabled(true)
  }

  /**
    * Clear the output.
    */
  def clearOutput(): Unit = {
    outputArea.setText("")
  }

  /**
    * Add output text.
    *
    * @param text the output text.
    */
  def addToOutput(text: String): Unit = {
    outputArea.append(text)
    outputArea.setEnabled(true)
    outputArea.setCaretPosition(outputArea.getText.length)
  }

  /**
    * Enable runtime input.
    */
  def enableInput(): Unit = {
    inputText.setEnabled(true)
    enterButton.setEnabled(true)
  }

  /**
    * Disable runtime input.
    */
  def disableInput(): Unit = {
    inputText.setEnabled(false)
    enterButton.setEnabled(false)
  }

  /**
    * Button event dispatcher.
    *
    * @param event the button event.
    */
  override def actionPerformed(event: ActionEvent): Unit = {
    val button = event.getSource
    if (button eq enterButton) enterAction()
  }

  /**
    * Enter button event handler.
    */
  private def enterAction(): Unit = {
    val text = inputText.getText + "\n"
    addToOutput(text)
    inputText.setText("")
    control.sendToDebuggerProcess(text)
  }
}
