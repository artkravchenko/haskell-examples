/*
  6. Напишите функцию,  обращающую каждый элемент
  списочной структуры (т.е. списки), а также саму
  списочную структуру. Например:

  [1,2,3,4,5,6,7,8,9] -> [9,8,7,6,5,4,3,2,1],
  [[1,2],[3],[4,5,6]] -> [[6,5,4],[3],[2,1]],
  [[[1,(-2),(-5),10]],[[(-3),(-2),1]]]
                    -> [[[1,(-2),(-3)]],[[10,(-5),(-2),1]]].
*/

implicit def anyToFileOutput(self: Any) = new {
  import java.io._
  def >>(filename: String) {
    val f = new BufferedWriter(new FileWriter(filename))
    try {
      f.write(self.toString)
    } finally {
      if (f != null)
        f.close()
    }
  }
}  

object Task6 {
  def reverse[T](lst: List[T]): List[T] = {
    def iterate(acc: List[T], rest: List[T]): List[T] = rest match {
      case Nil => acc
      case x: List[Any] => rest match {
        case Nil      => acc
        case _ :: Nil => (reverse (x)) :: acc
        case _ :: xs  => iterate ((reverse (x)) :: acc, xs)
      }
      case _ => rest match {
        case Nil      => acc
        case x :: Nil => x :: acc
        case x :: xs  => iterate (x :: acc, xs)
      }
    }
    iterate(List(), lst)
  }

  def main() = {
    try {
      reverse(List(1, 2, 3))
    } catch {
      case e: Exception => e.getMessage.apply(1)
    }
  }
}

trait Reverser[C] {
  def reverse(xs: C): C
}

implicit def rev[A](implicit ev: Reverser[A] = null) = if (ev == null) {
  new Reverser[List[A]] {
    def reverse(xs: List[A]) = Task6.reverse(xs)
  }
} else {
  new Reverser[List[A]] {
   def reverse(xs: List[A]) = Task6.reverse(xs(xs map Task6.reverse(xs))
  }
}

def deepReverse[A](xs: A)(implicit ev: Reverser[A]): A = ev.reverse(xs)
