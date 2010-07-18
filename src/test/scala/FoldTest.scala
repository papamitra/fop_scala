
import org.specs._

class FoldTest extends Specification{
  import Fold._

  val a = Cons(1,Cons(2,Cons(3,Nil)))
  val b = Cons(4,Cons(5,Cons(6,Nil)))
  val c = Cons(7,Cons(8,Cons(9,Nil)))

  "foldL" should {

    "(1,2,3)の足し算をfoldLで" in {
      foldL(0, a)(_ + _) must_==6
    }
  }

  "mapL" should {
    "(1,2,3)の各項を2倍" in{
      mapL(a)(_*2) must_== Cons(2,Cons(4,Cons(6,Nil)))
    }

    "(1,2,3)の各項を文字列に変換" in{
      mapL(a)(_.toString) must_== Cons("1",Cons("2",Cons("3",Nil)))
    }
  }

  "appendL" should {
    "(1,2,3) ++ (4,5,6) -> (1,2,3,4,5,6)" in{
      appendL(a,b) must_== Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Nil))))))
    }
  }

  "concatL" should {
    "List(a,b,c) -> (1,2,...,9)" in{
      concatL(Cons(a,Cons(b,Cons(c,Nil)))) must_== Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Cons(6,Cons(7,Cons(8,Cons(9,Nil)))))))))
    }
  }

  "isort" should {
    "(4,5,2,3,1) -> (1,2,3,4,5)" in{
      isort(Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
    }
  }

  "insert1" should {
    "insert1 y Nil => (Nil, wrap y)" in{
      insert1(1, Nil) must_==(Nil, wrap(1))
    }

    "insert 0 Cons(1,Nil) => (Cons(1,Nil), Cons(0,Cons(1,Nil)))" in {
      insert1(0,Cons(1,Nil))._1 must_== Cons(1,Nil)
      insert1(0,Cons(1,Nil))._2 must_== Cons(0,Cons(1,Nil))
    }
  }

  "isort2" should {
    "(4,5,2,3,1) -> (1,2,3,4,5)" in{
      isort2(Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
    }
  }
}
