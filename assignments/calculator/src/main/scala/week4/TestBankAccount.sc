import frp.Signal
import week4.BankAccount

def consolidated(accounts: List[BankAccount]): Signal[Int] = {
  Signal(accounts.map(_.balance()).sum)
}

val a = new BankAccount()
val b = new BankAccount()
val c = consolidated(List(a, b))

c()

a.deposit(20)

c()

b deposit 100

c()

a withdraw 10

c()

val xchange = Signal(246.0)
val inDollar = Signal(c() * xchange())

inDollar()

b withdraw 10

inDollar()