def from(i: Int): Stream[Int] = i #:: from(i + 1)

def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail filter (_ % s.head != 0))
}

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2

  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)

  guesses
}

def isGoodEnough(guess: Double, x: Double): Boolean = {
  math.abs((guess * guess - x) / x) < 0.0000000000001
}

val nats = from(0)
val m4s = nats map (_ * 4)
val primes = sieve(from(2))

(m4s take 3).toList
(primes take 10).toList

sqrtStream(4).filter(isGoodEnough(_, 4)).take(100).toList