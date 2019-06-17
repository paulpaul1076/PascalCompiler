package backend.compiler

class LocalStack {
  private var size = 0
  private var maxSize = 0

  def getSize: Int = size

  def increase(amount: Int) : Unit = {
    size += amount
    maxSize = Math.max(maxSize, size)
  }

  def decrease(amount: Int) : Unit = size -= amount

  def use(amount: Int): Unit = {
    increase(amount)
    decrease(amount)
  }

  def use(amountIncrease: Int, amountDecrease: Int): Unit ={
    increase(amountIncrease)
    decrease(amountDecrease)
  }

  def capacity(): Int = maxSize
}
