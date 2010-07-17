
import org.specs._

class HeapTest extends Specification{
  import BinaryHeap._

  "空のヒープ" should{
    "sizeは0" in{
      Null.size must_== 0
    }

    "3を追加するとFork(1,3,Null,Null)" in{
      Null.insert(3) must_==Fork(1,3,Null,Null)
    }
    "isEmpty == true" in{
      Null.isEmpty must_== true
    }
  }

  "Fork(1,1,Null,Null)" should{
    "2を追加するとFork(2,1,Fork(1,2,Null,Null),Null)" in{
      Fork(1,1,Null,Null).insert(2) must_== Fork(2,1,Fork(1,2,Null,Null),Null)
    }
  }
}
