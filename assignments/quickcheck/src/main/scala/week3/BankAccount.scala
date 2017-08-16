package week3

class BankAccount {
  var balance = 0

  def deposit(amount: Int): Unit = {
    if (amount > 0) balance = balance + amount
  }

  def withdraw(amount: Int): Int = {
    if (amount > 0 && amount <= balance) {
      balance = balance - amount
      balance
    } else throw new Error("Insuficient funds!")

  }
}
