
object BinaryHeap{
  sealed trait Tree{
    def size:Int
    def insert(v:Int):Tree
    def isEmpty:Boolean
  }

  object Null extends Tree{
    def size=0
    def insert(v:Int) = Fork(1,v,Null,Null)
    def isEmpty = true
  }

  case class Fork(depth:Int, v:Int, lhs:Tree, rhs:Tree) extends Tree{
    def size = depth
    def insert(v:Int) = merge(Fork(1,v,Null,Null), this)
    def isEmpty = false
    def minElem=v
  }

  def merge(a:Tree, b:Tree):Tree = (a,b) match{
    case (_, Null) => a
    case (Null, _) => b
    case (a:Fork, b:Fork) => if(a.minElem <= b.minElem) join(a,b) else join(b,a)
  }

  def join(a:Fork, b:Fork) = List(a.rhs, a.lhs, b) sortWith(_.size < _.size) match {
    case List(x,y,z) => Fork(a.size + b.size, a.v, z, merge(x,y))
  }

}
