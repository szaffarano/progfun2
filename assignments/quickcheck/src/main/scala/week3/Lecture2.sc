import week3.BankAccount

val x = new BankAccount
val y = new BankAccount
//val y = x

def test(b1: BankAccount, b2: BankAccount) = {
  b1 deposit 30
  b2 withdraw 20
}

test(x, x)
test(x, y)

x == y