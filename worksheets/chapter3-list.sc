import chapter3._
val l1 = List(2, 3, 4, 5, 6, 7, 8, 9, 10)
List.sum(l1)
List.sum2(l1)
val l2 = List(2.0, 4.0, 5.0)
List.product(l2)
List.product2(l2)

val l3 = List("2", "4", "5")

val x2 = List(1,2,3,4,5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}

List.tail(l3)

List.setHead("6", l3)
List.drop(l3, 2)
val l4 = List.dropWhile(l1)(_ < 6)

List.append(l1, l4)
List.append2(l1, l4)
List.append3(l1, l4)
List.init(l1)
List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
List.length(l1)
List.length(l2)
List.length(Nil)
List.foldLeft(l1, 0)(_ + _)
List.foldLeft2(l1, 0)(_ + _)
List.foldRight2(l1, 0)(_ + _)
List.reverse(l1)

val l6 = List(List(3,4,5), List(6,7,8), List(9,10,11))
List.flatten(l6)

List.map(l1)(_ + 1)
List.map(l2)(_.toString)

List.filter(l1)(_ % 2 == 0)
List.filter2(l1)(_ % 2 == 0)
List.filter3(l1)(_ % 2 == 0)
List.filter4(l1)(_ % 2 == 0)
List.flatMap(List(1, 2, 3))(i => List(i, i))

List.addLists(List(1,2,3), List(4,5,6))
List.zipWith(List(1,2,3), List(4,5,6))(_ + _)
List.zipWith(List(1,2,3), List(4,5,6))(_.toString + _.toString)

List.hasSubsequence(l1, List(3,4))
List.hasSubsequence(l1, List(3,5))
List.hasSubsequence(l1, List(5))
List.hasSubsequence(l1, List(7,8,9,10))