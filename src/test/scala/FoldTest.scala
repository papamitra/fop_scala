
import org.specs._

class FoldTest extends Specification{
  import Fold._

  val a = Cons(1,Cons(2,Cons(3,Nil)))
  val b = Cons(4,Cons(5,Cons(6,Nil)))
  val c = Cons(7,Cons(8,Cons(9,Nil)))

  "foldL" should {
    "(1,2,3)の足し算をfoldLで" in {
      foldL({(x:Int,y:Int)=>x+y},0, a) must_==6
    }
  }

  "foldL2" should {
    "(1,2,3)の足し算をfoldL2で" in {
      foldL2({(x:Int,y:Int)=>x+y},0, a) must_==6
    }
  }

  "foldL3" should {
    "(1,2,3)の足し算をfoldL3で" in {
      foldL3({(x:Int,y:Int)=>x+y},0, a) must_==6
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

  "isort3 from paraL" should {
    "(4,5,2,3,1) -> (1,2,3,4,5)" in{
      isort3(Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
    }
  }

  "minimumL" should {
    "(4,5,2,3,1) -> 1" in {
      minimumL[Int](Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_== 1
    }
  }

  "deleteL" should {
    "(4,5,2,3,1) delete 1 -> (4,5,2,3)" in{
      deleteL(1,Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Cons(4,Cons(5,Cons(2,Cons(3,Nil))))
    }
  }

  "delmin" should {
    "delmin (4,5,2,3,1) -> Some(1,(4,5,2,3))" in{
      delmin(Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Some(1,Cons(4,Cons(5,Cons(2,Cons(3,Nil)))))
    }
  }

  "ssort" should {
    "(4,5,2,3,1) -> (1,2,3,4,5)" in{
      ssort(Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
    }
  }

  // 練習問題3.10 deleteL2
  "deleteL2" should {
    "(4,5,2,3,1) delete 1 -> (4,5,2,3)" in{
      deleteL2(1,Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Cons(4,Cons(5,Cons(2,Cons(3,Nil))))
    }

    "(4,5,2,3,1) delete 5 -> (4,2,3,1)" in{
      deleteL2(5,Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Cons(4,Cons(2,Cons(3,Cons(1,Nil))))
    }
  }

  // 練習問題3.11 delminをparaLで
  "delmin2" should {
    "delmin (4,5,2,3,1) -> Some(1,(4,5,2,3))" in{
      delmin(Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Some(1,Cons(4,Cons(5,Cons(2,Cons(3,Nil)))))
    }
    
    "delmin Nil -> None" in {
      delmin(Nil) must_== None
    }
  }

  "bsort" should {
    "(4,5,2,3,1) -> (1,2,3,4,5)" in{
      bsort(Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
    }
  }

  "bsort2" should {
    "(4,5,2,3,1) -> (1,2,3,4,5)" in{
      bsort2(Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
    }
  }

  "isort4" should {
    "(4,5,2,3,1) -> (1,2,3,4,5)" in{
      isort4(Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
    }
  }

  "isort5" should {
    "(4,5,2,3,1) -> (1,2,3,4,5)" in{
      isort5(Cons(4,Cons(5,Cons(2,Cons(3,Cons(1,Nil)))))) must_==
      Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
    }
  }

  "insert5" should {
    "insert5 1 Nil => (1)" in{
      insert5(1, Nil) must_==Cons(1,Nil)
    }

    "insert 0 Cons(1,Nil) => Cons(0,Cons(1,Nil))" in {
      insert5(0,Cons(1,Nil)) must_== Cons(0,Cons(1,Nil))
    }

    "insert 2 (1, 3, 4, 5) => (1,2,3,4,5)" in {
      insert5(2,Cons(1,Cons(3,Cons(4,Cons(5,Nil))))) must_== Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
    }

  }
  
  "fact" should {
    "1 .. 10 fact" in{
      def f(n:Int):Int = if(n==0) 1 else n*f(n-1)
      for(i <- 1 to 10){
	fact(i) must_== f(i)
      }
    }
  }
}
