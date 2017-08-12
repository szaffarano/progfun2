val xs = Stream.cons(1, Stream.cons(2, Stream.Empty))
Stream(1, 2, 3)
(1 to 100000).toStream

def listRange(lo: Int, hi: Int): List[Int] = {
  if (lo > hi) Nil
  else lo :: listRange(lo + 1, hi)
}

def streamRange(lo: Int, hi: Int): Stream[Int] = {
  println(lo + " ")

  if (lo > hi) Stream.Empty
  //else Stream.cons(lo, streamRange(lo + 1, hi))
  else lo #:: streamRange(lo + 1, hi)
}

listRange(1, 10)
streamRange(1, 10) filter (p => p > 1)
streamRange(1, 1000).take(3).toList
