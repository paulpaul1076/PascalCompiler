package backend.compiler

class Directive(val text: String) {
  override def toString: String = text
}

object Directive {
  val CLASS_PUBLIC = new Directive(".class public")
  val END_CLASS = new Directive(".end class")
  val SUPER = new Directive(".super")
  val FIELD_PRIVATE_STATIC = new Directive(".field private static")
  val METHOD_PUBLIC = new Directive(".method public")
  val METHOD_STATIC = new Directive(".method static")
  val METHOD_PUBLIC_STATIC = new Directive(".method public static")
  val METHOD_PRIVATE_STATIC = new Directive(".method private static")
  val END_METHOD = new Directive(".end method")
  val LIMIT_LOCALS = new Directive(".limit locals")
  val LIMIT_STACK = new Directive(".limit stack")
  val VAR = new Directive(".var")
  val LINE = new Directive(".line")
}