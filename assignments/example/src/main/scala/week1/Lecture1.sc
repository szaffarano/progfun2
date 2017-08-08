case class Book(title: String, authors: List[String])

val books = Set(
  Book(title = "book1",
    authors = List("A1", "A2")),
  Book(title = "book2",
    authors = List("A1", "A3")),
  Book(title = "book3",
    authors = List("A1", "A4"))
)


// find books whose author is A1
for (b <- books; a <- b.authors if a startsWith "A1") yield b.title

// find books which have the world "book" in the title
for (b <- books if b.title.indexOf("book") >= 0) yield b.title

// find the names of all authors who have written at least two books
for (
  b1 <- books; b2 <- books
  if b1.title > b2.title;

  a1 <- b1.authors; a2 <- b2.authors
  if a1 == a2
) yield a1

val l1 = List("a", "b", "c")

l1.map(x => "_" + x)
l1.flatMap(x => List(x, "_"))
l1.filter(x => x > "a")
l1.withFilter(x => x > "a").foreach(a => println(a))d