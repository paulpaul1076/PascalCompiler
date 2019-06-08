package ide.ideimpl

import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.tree._
import ide.IDEControl
import javax.swing.tree.TreeSelectionModel._


/**
  * <h1>CallStackFrame</h1>
  *
  * <p>The call stack window of the Pascal IDE.</p>
  *
  * <p>Copyright (c) 2009 by Ronald Mak</p>
  * <p>For instructional purposes only.  No warranties.</p>
  */
class CallStackFrame() extends JInternalFrame with ActionListener with TreeSelectionListener {


  private val contentBorderLayout = new BorderLayout
  private val controlGridBagLayout = new GridBagLayout
  private val scrollPane = new JScrollPane
  private val controlPanel = new JPanel
  private var callTree = new JTree
  private val nameLabel = new JLabel
  private val nameText = new JTextField
  private val valueLabel = new JLabel
  private val valueText = new JTextField
  private val changeButton = new JButton
  private var control: IDEControl = _
  private var treeModel: DefaultTreeModel = _
  private var root: DefaultMutableTreeNode = _
  private var currentRoutineNode: DefaultMutableTreeNode = _
  private var routineNodeIndex = 0
  private var variableNodeIndex = 0


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
    changeButton.addActionListener(this)
  }

  /**
    * Initialize the GUI components.
    *
    * @throws Exception if an error occurred.
    */
  @throws[Exception]
  private[ideimpl] def initGuiComponents(): Unit = {
    this.setIconifiable(true)
    this.setMaximizable(true)
    this.setResizable(true)
    this.getContentPane.setLayout(contentBorderLayout)
    this.getContentPane.add(scrollPane, BorderLayout.CENTER)
    this.getContentPane.add(controlPanel, java.awt.BorderLayout.SOUTH)
    scrollPane.getViewport.add(callTree, null)
    callTree.setFont(new Font("Courier", 0, 12))
    callTree.setDoubleBuffered(true)
    callTree.setEditable(false)
    callTree.setEnabled(true)
    nameLabel.setText("Name:")
    nameText.setEditable(false)
    nameText.setText("")
    valueLabel.setText("Value:")
    valueText.setMinimumSize(new Dimension(45, 20))
    valueText.setPreferredSize(new Dimension(45, 20))
    valueText.setEditable(false)
    valueText.setText("")
    changeButton.setEnabled(false)
    changeButton.setText("Change")
    controlPanel.setBorder(BorderFactory.createLoweredBevelBorder)
    controlPanel.setLayout(controlGridBagLayout)
    controlPanel.add(nameLabel, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.BOTH, new Insets(5, 5, 0, 0), 0, 0))
    controlPanel.add(nameText, new GridBagConstraints(1, 0, 2, 1, 1.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 0, 5), 2, 0))
    controlPanel.add(valueLabel, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0, GridBagConstraints.EAST, GridBagConstraints.BOTH, new Insets(5, 5, 5, 0), 0, 0))
    controlPanel.add(valueText, new GridBagConstraints(1, 1, 1, 1, 1.0, 0.0, GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 5, 0), 0, 0))
    controlPanel.add(changeButton, new GridBagConstraints(2, 1, 1, 1, 0.0, 0.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(5, 5, 5, 5), 0, 0))
  }

  /**
    * Initialize the call stack tree.
    */
  def initialize(): Unit = { // Remove the old tree from the scroll pane.
    if (callTree != null) scrollPane.getViewport.remove(callTree)
    // Create a new tree.
    root = new DefaultMutableTreeNode("Most recent invocation on top.")
    treeModel = new DefaultTreeModel(root)
    callTree = new JTree(treeModel)
    callTree.getSelectionModel.setSelectionMode(SINGLE_TREE_SELECTION)
    callTree.addTreeSelectionListener(this)
    nameText.setText("")
    valueText.setText("")
    changeButton.setEnabled(false)
    routineNodeIndex = 0
  }

  /**
    * Add an invoked routine to the call stack tree.
    *
    * @param level  the routine's nesting level.
    * @param header the routine's header.
    */
  def addRoutine(level: String, header: String): Unit = {
    val info = new RoutineNodeInfo(level, header)
    currentRoutineNode = new DefaultMutableTreeNode(info)
    treeModel.insertNodeInto(currentRoutineNode, root, {
      routineNodeIndex += 1; routineNodeIndex - 1
    })
    variableNodeIndex = 0
  }

  /**
    * Add a local variable to the call stack tree.
    *
    * @param name  the variable's name.
    * @param value the variable's value.
    */
  def addVariable(name: String, value: String): Unit = {
    val info = new VariableNodeInfo(name, value)
    val variableNode = new DefaultMutableTreeNode(info)
    treeModel.insertNodeInto(variableNode, currentRoutineNode, {
      variableNodeIndex += 1; variableNodeIndex - 1
    })
  }

  /**
    * Complete the call stack tree.
    */
  def complete(): Unit = { // Expand all the nodes.
    var i = 0
    while ( {
      i < callTree.getRowCount
    }) {
      callTree.expandRow(i)

      {
        i += 1; i
      }
    }
    // Insert the new tree into the scroll pane.
    scrollPane.getViewport.add(callTree, null)
  }

  /**
    * Button event dispatcher.
    *
    * @param event the button event.
    */
  override def actionPerformed(event: ActionEvent): Unit = {
    val button = event.getSource
    if (button eq changeButton) changeButtonAction()
  }

  /**
    * Change button event handler.
    */
  private def changeButtonAction(): Unit = {
    control.sendToDebuggerProcess("assign " + nameText.getText + " " + valueText.getText + ";")
  }

  /**
    * Tree selection event handler.
    *
    * @param event the selection event.
    */
  override def valueChanged(event: TreeSelectionEvent): Unit = {
    val path = event.getPath
    val node = path.getPath()(path.getPathCount - 1).asInstanceOf[DefaultMutableTreeNode]
    val info = node.getUserObject
    // Only operate on variable nodes.
    if (info.isInstanceOf[CallStackFrame#VariableNodeInfo]) {
      nameText.setText(info.asInstanceOf[CallStackFrame#VariableNodeInfo].name)
      valueText.setText(info.asInstanceOf[CallStackFrame#VariableNodeInfo].value)
      // Allow changing only the values of scalar variables.
      val firstChar = info.asInstanceOf[CallStackFrame#VariableNodeInfo].value.charAt(0)
      val changeable = (firstChar != '[') && (firstChar != '{')
      valueText.setEditable(changeable)
      changeButton.setEnabled(changeable)
    }
  }

  /**
    * Info for a routine tree node.
    */
  private class RoutineNodeInfo(var level: String // routine's nesting level
                                        , var header: String // routine's header string
                                       ) {

  /**
    * Constructor.
    *
    * @param level  the routine's nesting level.
    * @param header the routine's header string.
    */
    /**
      * @return the node label.
      */
    override def toString(): String = {
      return level + " " + header
    }
  }

  /**
    * Info for a variable tree node.
    */
  private class VariableNodeInfo(var name: String, var value: String) {

  /**
    * Constructor.
    *
    * @param name  the variable's name.
    * @param value the variable's value.
    */
//    override def toString(): String = {
//      return name + ": " + value
//    }

    override def toString: String = {
      return name + ": " + value
    }
  }

}
