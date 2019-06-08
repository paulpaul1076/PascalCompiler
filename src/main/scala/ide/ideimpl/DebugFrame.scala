package ide.ideimpl

import java.util._
import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.border._
import java.awt.BorderLayout
import ide.IDEControl


/**
  * <h1>DebugFrame</h1>
  *
  * <p>The debug window of the Pascal IDE.</p>
  *
  * <p>Copyright (c) 2009 by Ronald Mak</p>
  * <p>For instructional purposes only.  No warranties.</p>
  */
object DebugFrame {
  private val INIT_AUTO_STEP_WAIT_TIME = 200
}

class DebugFrame() extends JInternalFrame with ActionListener {


  private val contentBorderLayout = new BorderLayout
  private val messageBorderLayout = new BorderLayout
  private var messageTitledBorder: TitledBorder = _
  private val controlGridBagLayout = new GridBagLayout
  private val buttonGridLayout = new GridLayout
  private val listingScrollPane = new JScrollPane
  private val listing = new JList[Any]
  private val controlPanel = new JPanel
  private val messagePanel = new JPanel
  private val messageArea = new JTextArea
  private val buttonPanel = new JPanel
  private val filler1Panel = new JPanel
  private val goButton = new JButton
  private val quitButton = new JButton
  private val singleStepButton = new JButton
  private val autoStepButton = new JButton
  private val slowerButton = new JButton
  private val fasterButton = new JButton
  private var control: IDEControl = _
  private var autoStepper: DebugFrame#AutoStepper = _
  private var currentLineNumber = 0
  private var autoStepWaitTime = 0
  private var programRunning = false
  private var settingBreakpoint = false
  private var autoStepping = false
  private var atCommandPrompt = false
  private var breakpointFlippable = false
  private var savedSingleStepState = false
  private val lineBuffer = new Vector[DebugFrame#ListingLine] // listing lines buffer

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
    goButton.addActionListener(this)
    quitButton.addActionListener(this)
    singleStepButton.addActionListener(this)
    autoStepButton.addActionListener(this)
    slowerButton.addActionListener(this)
    fasterButton.addActionListener(this)
    listing.setCellRenderer(new ListingLineRenderer)
    // Mouse click listener for the listing.
    listing.addMouseListener(new MouseAdapter() {
      override def mouseClicked(event: MouseEvent): Unit = {
        if (programRunning) {
          settingBreakpoint = true
          toggleBreakpoint(listing.locationToIndex(event.getPoint))
          settingBreakpoint = false
        }
        listing.clearSelection()
        selectListingLine(currentLineNumber)
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
    messageTitledBorder = new TitledBorder(BorderFactory.createLineBorder(new Color(153, 153, 153), 2), "Interpreter messages")
    this.getContentPane.setLayout(contentBorderLayout)
    this.setIconifiable(true)
    this.setMaximizable(true)
    this.setResizable(true)
    this.getContentPane.add(listingScrollPane, BorderLayout.CENTER)
    this.getContentPane.add(controlPanel, BorderLayout.SOUTH)
    listing.setFont(new Font("Courier", 0, 12))
    listing.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    controlPanel.setLayout(controlGridBagLayout)
    messagePanel.setLayout(messageBorderLayout)
    messagePanel.setBorder(messageTitledBorder)
    messagePanel.setMinimumSize(new Dimension(12, 50))
    messagePanel.setPreferredSize(new Dimension(12, 50))
    buttonGridLayout.setRows(2)
    buttonGridLayout.setColumns(3)
    buttonGridLayout.setHgap(5)
    buttonGridLayout.setVgap(5)
    messagePanel.add(messageArea, BorderLayout.CENTER)
    messageArea.setFont(new Font("Dialog", 1, 12))
    messageArea.setForeground(Color.blue)
    messageArea.setEditable(false)
    messageArea.setText("")
    buttonPanel.setLayout(buttonGridLayout)
    buttonPanel.setBorder(BorderFactory.createLoweredBevelBorder)
    filler1Panel.setLayout(null)
    goButton.setEnabled(false)
    goButton.setText("Go")
    quitButton.setEnabled(false)
    quitButton.setText("Quit")
    singleStepButton.setEnabled(false)
    singleStepButton.setText("Single step")
    autoStepButton.setEnabled(false)
    autoStepButton.setText("Auto step")
    fasterButton.setEnabled(false)
    fasterButton.setText("Faster stepping")
    slowerButton.setEnabled(false)
    slowerButton.setText("Slower stepping")
    listingScrollPane.getViewport.add(listing, null)
    controlPanel.add(messagePanel, new GridBagConstraints(0, 0, 4, 1, 1.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 0, 5), 0, 0))
    controlPanel.add(buttonPanel, new GridBagConstraints(0, 1, 1, 1, 1.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 5, 5), 0, 0))
    buttonPanel.add(goButton, null)
    buttonPanel.add(singleStepButton, null)
    buttonPanel.add(slowerButton, null)
    buttonPanel.add(quitButton, null)
    buttonPanel.add(autoStepButton, null)
    buttonPanel.add(fasterButton, null)
  }

  /**
    * Initialize the control panel buttons.
    */
  private def initializeButtons(): Unit = {
    goButton.setEnabled(true)
    quitButton.setEnabled(true)
    savedSingleStepState = true
    singleStepButton.setEnabled(savedSingleStepState)
    autoStepButton.setEnabled(true)
    slowerButton.setEnabled(false)
    fasterButton.setEnabled(false)
  }

  /**
    * Disable the control panel buttons.
    */
  private def disableButtons(): Unit = {
    savedSingleStepState = singleStepButton.isEnabled
    goButton.setEnabled(false)
    singleStepButton.setEnabled(false)
  }

  /**
    * Restore the control panel buttons.
    */
  private def restoreButtons(): Unit = {
    goButton.setEnabled(true)
    singleStepButton.setEnabled(savedSingleStepState)
  }

  /**
    * Initialize the debugger window.
    */
  def initialize(): Unit = {
    programRunning = true
    atCommandPrompt = false
    autoStepping = false
    autoStepWaitTime = DebugFrame.INIT_AUTO_STEP_WAIT_TIME
    initializeButtons()
  }

  /**
    * Clear the listing lines.
    */
  def clearListing(): Unit = {
    lineBuffer.clear()
    loadListing()
  }

  /**
    * Add a listing line.
    *
    * @param line the line.
    */
  def addListingLine(line: String): Unit = {
    lineBuffer.addElement(new ListingLine(line, true))
    loadListing()
  }

  /**
    * Select a listing line.
    *
    * @param lineNumber the line number.
    */
  def selectListingLine(lineNumber: Int): Unit = {
    classOf[DebugFrame].synchronized {
      currentLineNumber = lineNumber
    }
    val index = lineNumber - 1
    listing.setSelectedIndex(index)
    listing.ensureIndexIsVisible(index)

  }

  /**
    * Set the debugger to a listing line.
    *
    * @param lineNumber the line number.
    */
  def atListingLine(lineNumber: Int): Unit = {
    atCommandPrompt = true
    breakpointFlippable = true
    selectListingLine(lineNumber)
    restoreButtons()
    if (autoStepping) {
      goButton.setEnabled(false)
      autoStepButton.setEnabled(true)
      slowerButton.setEnabled(true)
      fasterButton.setEnabled(autoStepWaitTime > 0)
    }
  }

  /**
    * Set the debugger to break at a listing line.
    *
    * @param lineNumber the line number.
    */
  def breakAtListingLine(lineNumber: Int): Unit = {
    autoStepping = false
    breakpointFlippable = true
    selectListingLine(lineNumber)
    initializeButtons()
  }

  /**
    * Set the message.
    *
    * @param message the message.
    * @param color   the message color.
    */
  def setMessage(message: String, color: Color): Unit = {
    messageArea.setForeground(color)
    messageArea.setText(message)
  }

  /**
    * Stop the debugger.
    */
  def stop(): Unit = {
    programRunning = false
    autoStepping = false
    goButton.setEnabled(false)
    singleStepButton.setEnabled(false)
    autoStepButton.setEnabled(false)
    slowerButton.setEnabled(false)
    fasterButton.setEnabled(false)
    quitButton.setEnabled(false)
    listing.repaint()
  }

  /**
    * Load the listing lines.
    */
  private def loadListing(): Unit = {
    classOf[DebugFrame] synchronized listing.clearSelection()
    listing.setListData(lineBuffer)

  }

  /**
    * Clear all the breakpoints.
    */
  private def clearAllBreakpoints(): Unit = {
    val model = listing.getModel
    val size = model.getSize
    var i = 0
    while ( {
      i < size
    }) {
      val line = model.getElementAt(i).asInstanceOf[DebugFrame#ListingLine]
      if (line.breakpoint) {
        line.breakpoint = false
        val lineNumber = i + 1
        control.sendToDebuggerProcess("unbreak " + lineNumber + ";")
      }

      {
        i += 1; i
      }
    }
    listing.repaint()
  }

  /**
    * Toggle the breakpoint on the listing line the mouse clicked on.
    *
    * @param index the source line index.
    */
  private def toggleBreakpoint(index: Int): Unit = {
    val line = listing.getModel.getElementAt(index).asInstanceOf[DebugFrame#ListingLine]
    if (breakpointFlippable && line.isBreakable) {
      line.toggleBreakpoint()
      val command = if (line.isBreakpoint) "break "
      else "unbreak "
      val lineNumber = index + 1
      // Execute the break or unbreak command.
      control.sendToDebuggerProcess(command + lineNumber + ";")
    }
  }

  /**
    * Button event dispatcher.
    *
    * @param event the button event.
    */
  override def actionPerformed(event: ActionEvent): Unit = {
    val button = event.getSource
    if (button eq goButton) goAction()
    else if (button eq quitButton) quitAction()
    else if (button eq singleStepButton) singleStepAction()
    else if (button eq autoStepButton) autoStepAction()
    else if (button eq slowerButton) slowerAction()
    else if (button eq fasterButton) fasterAction()
  }

  /**
    * Go button event handler.
    */
  private def goAction(): Unit = {
    programRunning = true
    disableButtons()
    breakpointFlippable = false
    control.sendToDebuggerProcess("go;")
    control.enableConsoleWindowInput
  }

  /**
    * Quit button event handler.
    */
  private def quitAction(): Unit = {
    programRunning = false
    clearAllBreakpoints()
    goButton.setEnabled(false)
    quitButton.setEnabled(false)
    singleStepButton.setEnabled(false)
    autoStepButton.setEnabled(false)
    slowerButton.setEnabled(false)
    fasterButton.setEnabled(false)
    control.sendToDebuggerProcess("quit;")
    control.stopDebuggerProcess
  }

  /**
    * Single step button event handler.
    */
  private def singleStepAction(): Unit = {
    disableButtons()
    breakpointFlippable = false
    control.sendToDebuggerProcess("step;")
    control.sendToDebuggerProcess("stack;")
    control.enableConsoleWindowInput
  }

  /**
    * Autostep button event handler.
    */
  private def autoStepAction(): Unit = { // Stop autostepping.
    if (autoStepping) autoStepping = false
    else { // Start autostepping.
      autoStepper = new AutoStepper
      goButton.setEnabled(false)
      savedSingleStepState = false
      singleStepButton.setEnabled(savedSingleStepState)
      autoStepButton.setText("Stop stepping")
      autoStepButton.setEnabled(true)
      slowerButton.setEnabled(true)
      fasterButton.setEnabled(autoStepWaitTime > 0)
      autoStepping = true
      autoStepper.start()
    }
  }

  /**
    * Slower stepping button event handler.
    */
  private def slowerAction(): Unit = {
    autoStepWaitTime += 50
    fasterButton.setEnabled(true)
  }

  /**
    * Faster stepping button event handler.
    */
  private def fasterAction(): Unit = {
    autoStepWaitTime -= 50
    if (autoStepWaitTime <= 0) autoStepWaitTime = 0
    fasterButton.setEnabled(autoStepWaitTime > 0)
  }

  /**
    * Autostepper thread.
    */
  private class AutoStepper extends Thread {
    override def run(): Unit = { // Loop to perform a single-step action for each command prompt
      // until autoStepping is false. Sleep between each single step.
      do try
          if (atCommandPrompt) {
            atCommandPrompt = false
            singleStepAction()
            Thread.sleep(autoStepWaitTime)
          }
          else Thread.`yield`()
      catch {
        case ignore: Exception =>

      } while ( {
        autoStepping
      })
      goButton.setEnabled(programRunning)
      quitButton.setEnabled(programRunning)
      savedSingleStepState = programRunning
      singleStepButton.setEnabled(savedSingleStepState)
      autoStepButton.setText("Auto step")
      autoStepButton.setEnabled(programRunning)
      slowerButton.setEnabled(false)
      fasterButton.setEnabled(false)
    }
  }

  /**
    * The listing line.
    */
  class ListingLine(textt: String, breakablee: Boolean){

  /**
    * Constructor.
    *
    * @param text      the line text.
    * @param breakablee true if can set a breakpoint here, else false.
    */
    this.breakable = breakable
    var breakable = true
    var breakpoint = false

    /**
      * Getter.
      *
      * @return the line text.
      */
    def getText = textt

    /**
      * @return true if breakable, else false.
      */
    def isBreakable = breakable

    /**
      * @return true if breakpoint set, else false.
      */
    def isBreakpoint: Boolean = breakpoint

    /**
      * Toggle the breakpoint.
      */
    def toggleBreakpoint(): Unit = {
      this.breakpoint = !this.breakpoint
    }
  }

  private val listingFont = new Font("Courier", Font.PLAIN, 12)
  private val nobreakIcon = new ImageIcon("nobreak.gif")
  private val breakableIcon = new ImageIcon("breakable.gif")
  private val breakPointIcon = new ImageIcon("breakpoint.gif")

  /**
    * Listing line renderer.
    */
  private class ListingLineRenderer private[ideimpl]()

  /**
    * Constructor.
    */
    extends JLabel with ListCellRenderer[Any] {
    setOpaque(true)

    /**
      * Return the renderer component.
      *
      * @param list         the JList object.
      * @param value        the listing line object.
      * @param index        the list index.
      * @param isSelected   true if selected, else false.
      * @param cellHasFocus true if has focus, else false.
      * @return the renderer component.
      */
    override def getListCellRendererComponent(list: JList[_], value: Any, index: Int, isSelected: Boolean, cellHasFocus: Boolean): Component = {
      val line = value.asInstanceOf[ListingLine]
      val icon = if (!line.isBreakable) nobreakIcon
      else if (!programRunning) nobreakIcon
      else if (line.isBreakpoint) breakPointIcon
      else breakableIcon
      setIcon(icon)
      setFont(listingFont)
      setText(line.getText)
      setForeground(if (line.isBreakpoint) Color.RED
      else Color.BLACK)
      setBackground(if (isSelected && !settingBreakpoint) Color.LIGHT_GRAY
      else Color.WHITE)
      this
    }
  }

}
