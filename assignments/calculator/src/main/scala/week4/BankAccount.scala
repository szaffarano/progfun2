package week4

import frp.Var

class BankAccount {
  val balance: Var[Int] = new Var(0)

  def deposit(amount: Int): Unit = {
    if (amount > 0) {
      val value = balance()
      balance() = value + amount
    }
  }

  def withdraw(amount: Int): Unit = {
    if (0 < amount && amount <= balance()) {
      val value = balance()
      balance() = value - amount
    } else {
      throw new Error("Insufficient funds!")
    }
  }
}
