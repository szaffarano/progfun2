import java.util.Random

val rand = new Random
rand.nextInt
rand.nextBoolean

trait Generator[+T] {
  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate = f(self.generate).generate
  }
}

val integers = new Generator[Int] {
  val rand = new Random

  def generate = rand.nextInt
}

val booleans = new Generator[Boolean] {
  def generate = integers.generate > 0
}
val booleans2 = for (x <- integers) yield x > 0


val pairs = new Generator[(Int, Int)] {
  def generate = (integers.generate, integers.generate)
}