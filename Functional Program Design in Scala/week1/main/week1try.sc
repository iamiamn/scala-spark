
case class Book(title:String, authors:List[String])
val books: List[Book] = List(
  Book(title = "Structure and Interpretation of Computer Programs",
    authors = List("Abelson, Harald", "Sussman, Gerald J.")),
  Book(title = "Introduction to Functional Programming",
    authors = List("Bird, Richard", "Wadler, Phil")),
  Book(title = "Effective Java",
    authors = List("Bloch, Joshua")),
  Book(title = "Java Puzzlers",
    authors = List("Bloch, Joshua", "Gafter, Neal")),
  Book(title = "Programming in Scala",
    authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))

def findUpTwice(books:List[Book]):List[String] = {
  //找出全部写过两本以上书的作者
  for{
    b1 <- books
    b2 <- books
    if b1 != b2
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield(a1)
}

val a1 = findUpTwice(books)

val bookSet = books.toSet
for {
  b1 <- bookSet;
  b2 <- bookSet;
//  println(b1, b2);//不知道为什么这句就是得不到
  if b1.title < b2.title
  a1 <- b1.authors
  a2 <- b2.authors
  if a1 == a2
} yield a1 // for的结果类型是由传入bookSet决定，Bookset为List则结果为list

def ttry(book1:Book, book2:Book) :Boolean = {
  println(book1.title, book2.title)
  true
}
for {
  b1 <- bookSet;
  b2 <- bookSet;
  if (ttry(b1, b2))
} yield(b1, b2)


case class Empty[T]() extends Cons[T]{
  def head:Nothing = throw new NoSuchElementException
  def tail:Nothing  = throw new NoSuchElementException
}
case class NoEmpty[T](xs:List[T]) extends Cons[T]{
 def head = xs(0)
 def tail:Cons[T] = xs.tail match{
    case  y::ys => NoEmpty(xs.tail)//
    case Nil => Empty()
  }
}
trait Cons[+T]{
  def head:T
  def tail:Cons[T]
  def show: String = this.tail match{
    case NoEmpty(xs) => this.head.toString + "," + this.tail.show
    case Empty() => ""
  }
}
val c1 = NoEmpty(List(1,2,3))
c1.show
