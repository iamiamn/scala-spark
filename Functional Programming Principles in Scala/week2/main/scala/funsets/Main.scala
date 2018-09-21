package funsets

object Main extends App {
  import FunSets._
//  println(contains(singletonSet(1), 1))

//  println(contains(x => (x<0), 100))

//  Set s1 = filter(y => y <= 1000, x => (x>= -1000))
  println(forall(x => (x<10), x => (x<20)))
  println(forall(x => (x >= -99), x => (x <= 1000)))
  println(forall(x => (x >= -99), x => (x <= 100)))
//  println(toString(filter(x => (x >= - 99 ), x => (x <= 100 ))))
  printSet(filter(x => (x >= - 9900 ), x => (x <= -999 )))
  printSet(map(x => x >0, x => x*x*x  ))
  printSet(x => x%2 == 0)
  printSet(x => x%2 == 1)
  printSet(x => x%2 == -1)//attention difference from remainder equal to 1
  printSet((x$14: Int) => x$14 < -998)
//  print()
  println(forall(x => x%2 == 1, ((x$14: Int) => x$14.%(2).==(0))))
  println(exists(((x$14: Int) => x$14 < -998), x => x%2 == 1))
  println(exists(((x$14: Int) => x$14 < -998), x => x%2 == -1))
}


