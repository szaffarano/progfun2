import org.scalacheck.Prop.{BooleanOperators, all, forAll}
import org.scalacheck.{Arbitrary, Gen, Properties}

val testGen = for {
  i <- Arbitrary.arbitrary[Int]
  j <- Gen.const(1)
} yield (i, j)

testGen.sample

val testSqr = forAll {
  n: Int =>
    (n > 0) ==> (scala.math.sqrt(n.toLong * n.toLong) == n)
}
testSqr.check

object StringSpecification extends Properties("String") {
  property("startWith") = forAll {
    (s1: String, s2: String) => (s1 + s2).startsWith(s1)
  }
  property("endsWith") = forAll {
    (s1: String, s2: String) => (s1 + s2).endsWith(s2)
  }
}

object IntSpecification extends Properties("Int") {
  property("squareRoot") = forAll {
    n: Int =>
      (n > 0) ==> (scala.math.sqrt(n.toLong * n.toLong) == n)
  }

  property("playingWithLabels") = forAll {
    (n: Int, m: Int) =>
      (n + m == m + n) :| "comutativity"
  }

  property("mul") = forAll { (n: Int, m: Int) =>
    val res = n * m
    ("evidence = " + res) |: all(
      "div1" |: m != 0 ==> (res / m == n),
      "div2" |: n != 0 ==> (res / n == m),
      "lt1" |: res > m,
      "lt2" |: res > n
    )
  }
}

object AppSpec extends Properties("App") {
  include(StringSpecification)
  include(IntSpecification)
}

AppSpec.check