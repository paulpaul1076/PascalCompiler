package backend.compiler

object Instruction {

  // Load constants 0,1,2, ..., -1 onto stack
  val ICONST_0 = new Instruction("ICONST_0")
  val ICONST_1 = new Instruction("ICONST_1")
  val ICONST_2 = new Instruction("ICONST_2")
  val ICONST_3 = new Instruction("ICONST_3")
  val ICONST_4 = new Instruction("ICONST_4")
  val ICONST_5 = new Instruction("ICONST_5")
  val ICONST_M1 = new Instruction("ICONST_M1")

  val FCONST_0 = new Instruction("FCONST_0")
  val FCONST_1 = new Instruction("FCONST_1")
  val FCONST_2 = new Instruction("FCONST_2")

  val ACONST_NULL = new Instruction("ACONST_NULL")
  val BIPUSH = new Instruction("BIPUSH") // load values from -128 to 127 onto stack
  val SIPUSH = new Instruction("SIPUSH") // load values from -2^15 to 2^15-1 onto the stack
  val LDC = new Instruction("LDC") // load any values onto the stack

  // Load value of address
  val ILOAD_0 = new Instruction("ILOAD_0")
  val ILOAD_1 = new Instruction("ILOAD_1")
  val ILOAD_2 = new Instruction("ILOAD_2")
  val ILOAD_3 = new Instruction("ILOAD_3")

  val FLOAD_0 = new Instruction("FLOAD_0")
  val FLOAD_1 = new Instruction("FLOAD_1")
  val FLOAD_2 = new Instruction("FLOAD_2")
  val FLOAD_3 = new Instruction("FLOAD_3")

  val ALOAD_0 = new Instruction("ALOAD_0")
  val ALOAD_1 = new Instruction("ALOAD_1")
  val ALOAD_2 = new Instruction("ALOAD_2")
  val ALOAD_3 = new Instruction("ALOAD_3")

  val ILOAD = new Instruction("ILOAD")
  val FLOAD = new Instruction("FLOAD")
  val ALOAD = new Instruction("ALOAD")

  val GETSTATIC = new Instruction("GETSTATIC") // getstatic Newton/number I pushes the static field number of class Newton on top of the stack as an integer
  val GETFIELD = new Instruction("GETFIELD")

  // Store value or address into array from the top of the stack.
  val ISTORE_0 = new Instruction("ISTORE_0")
  val ISTORE_1 = new Instruction("ISTORE_1")
  val ISTORE_2 = new Instruction("ISTORE_2")
  val ISTORE_3 = new Instruction("ISTORE_3")

  val FSTORE_0 = new Instruction("FSTORE_0")
  val FSTORE_1 = new Instruction("FSTORE_1")
  val FSTORE_2 = new Instruction("FSTORE_2")
  val FSTORE_3 = new Instruction("FSTORE_3")

  val ASTORE_0 = new Instruction("ASTORE_0")
  val ASTORE_1 = new Instruction("ASTORE_1")
  val ASTORE_2 = new Instruction("ASTORE_2")
  val ASTORE_3 = new Instruction("ASTORE_3")

  val ISTORE = new Instruction("ISTORE")
  val FSTORE = new Instruction("FSTORE") // FSTORE 5 pop the floating point value at the top of the stack and stores it in the local vars array at index 5
  val ASTORE = new Instruction("ASTORE")

  val PUTSTATIC = new Instruction("PUTSTATIC")
  val PUTFIELD = new Instruction("PUTFIELD")

  // Operand stack
  val POP = new Instruction("POP")
  val SWAP = new Instruction("SWAP")
  val DUP = new Instruction("DUP")

  // Arithmetic and logical
  val IADD = new Instruction("IADD") // adds 2 vars at the top of the stack, pushes the result back on top of the stack
  val FADD = new Instruction("FADD")
  val ISUB = new Instruction("ISUB")
  val FSUB = new Instruction("FSUB")
  val IMUL = new Instruction("IMUL")
  val FMUL = new Instruction("FMUL")
  val IDIV = new Instruction("IDIV")
  val FDIV = new Instruction("FDIV")
  val IREM = new Instruction("IREM")
  val FREM = new Instruction("FREM")
  val INEG = new Instruction("INEG")
  val FNEG = new Instruction("FNEG")
  val IINC = new Instruction("IINC")
  val IAND = new Instruction("IAND")
  val IOR = new Instruction("IOR")
  val IXOR = new Instruction("IXOR")

  // Type conversion and checking
  val I2F = new Instruction("I2F")
  val I2C = new Instruction("I2C")
  val I2D = new Instruction("I2D")
  val F2I = new Instruction("F2I")
  val F2D = new Instruction("D2F")
  val D2F = new Instruction("D2F")
  val CHECKCAST = new Instruction("CHECKCAST")

  // Objects and arrays
  val NEW = new Instruction("NEW")
  val NEWARRAY = new Instruction("NEWARRAY")
  val ANEWARRAY = new Instruction("ANEWARRAY")
  val MULTIANEWARRAY = new Instruction("MULTIANEWARRAY")
  // load from array onto the stack the second letter A means that they work with Arrays.
  val IALOAD = new Instruction("IALOAD")
  val FALOAD = new Instruction("FALOAD")
  val BALOAD = new Instruction("BALOAD")
  val CALOAD = new Instruction("CALOAD")
  val AALOAD = new Instruction("AALOAD")
  val IASTORE = new Instruction("IASTORE")
  val FASTORE = new Instruction("FASTORE")
  val BASTORE = new Instruction("BASTORE")
  val CASTORE = new Instruction("CASTORE")
  val AASTORE = new Instruction("AASTORE")

  // Compare and branch
  val IFEQ = new Instruction("IFEQ")
  val IFNE = new Instruction("IFNE")
  val IFLT = new Instruction("IFLT")
  val IFLE = new Instruction("IFLE")
  val IFGT = new Instruction("IFGT")
  val IFGE = new Instruction("IFGE")
  val IF_ICMPEQ = new Instruction("IF_ICMPEQ")
  val IF_ICMPNE = new Instruction("IF_ICMPNE")
  val IF_ICMPLT = new Instruction("IF_ICMPLT")
  val IF_ICMPLE = new Instruction("IF_ICMPLE")
  val IF_ICMPGT = new Instruction("IF_ICMPGT")
  val IF_ICMPGE = new Instruction("IF_ICMPGE")
  val FCMPG = new Instruction("FCMPG")
  val GOTO = new Instruction("GOTO")
  val LOOKUPSWITCH = new Instruction("LOOKUPSWITCH")

  // Call and return
  val INVOKESTATIC = new Instruction("INVOKESTATIC")
  val INVOKEVIRTUAL = new Instruction("INVOKEVIRTUAL")
  val INVOKENONVIRTUAL = new Instruction("INVOKENONVIRTUAL")
  val RETURN = new Instruction("RETURN")
  val IRETURN = new Instruction("IRETURN")
  val FRETURN = new Instruction("FRETURN")
  val ARETURN = new Instruction("FRETURN")

  // No operation
  val NOP = new Instruction("NOP")


}

class Instruction(val string: String) {
  override def toString: String = string
}
