
import org.specs._

class NaturalNumberTest extends Specification{
  import NaturalNumber._

  "foldN" should{
    "Nato to Int" in{
      foldN[Int](0, _+1, 10) must_== 10
    }
  }

  "練習問題3.16" should{
    "foldNpで定義されたfoldN2はfoldNと同じように振舞う" in {
      foldN[Int](0, _+1, 10) must_== 10
    }
  }

  "練習問題3.17" should {
    "addN" in {
      addN(3,4) must_== num2Nat(7)
    }

    "mulN" in {
      mulN(3,4) must_== num2Nat(12)
    }

    "mulNでZeroかけるとZeroになるか" in{
      mulN(Zero, 4) must_== Zero
      mulN(4,Zero) must_== Zero
    }

    "powN" in {
      powN(3,4) must_== num2Nat(81)
    }
  }

  
  "練習問題3.19" should {
    "predN と predN_from_foldNは等しくなるはず" in{
      predN_from_foldN(4) must_== Some(num2Nat(3))
      predN(4) must_== predN_from_foldN(4)
      predN(0) must_== predN_from_foldN(0)
    }
  }

  "練習問題3.20" should {
    "subN" in {
      subN(4,2) must_== Some(num2Nat(2))
      subN(2,4) must_== None
    }
  }

}
