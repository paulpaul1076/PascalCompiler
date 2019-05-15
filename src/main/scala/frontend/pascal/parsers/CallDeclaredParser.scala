package frontend.pascal.parsers

import frontend.pascal.PascalParserTD
import frontend.{Parser, Token}
import intermediate.icodeimpl.{ICodeKeyImpl, ICodeNodeTypeImpl}
import intermediate.{ICodeFactory, ICodeNode}

class CallDeclaredParser(parent: PascalParserTD) extends CallParser(parent) {
  override def parse(toket: Token): ICodeNode = {
    var curToken = toket
    // Create the CALL node.
    val callNode = ICodeFactory.createICodeNode(ICodeNodeTypeImpl.CALL)
    val pfId = Parser.symTabStack.lookup(curToken.getText.toLowerCase)
    callNode.setAttribute(ICodeKeyImpl.ID, pfId)
    callNode.setTypeSpec(pfId.getTypeSpec)

    curToken = nextToken() // consume procedure or function identifier

    val parmsNode = parseActualParameters(curToken, pfId, true, false, false)
    callNode.addChild(parmsNode)

    callNode
  }
}
