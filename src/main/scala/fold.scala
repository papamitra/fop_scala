
object Fold{
  sealed trait List[+T]

  object Nil extends List[Nothing]

  case class Cons[+T](val a:T, l:List[T]) extends List[T]

  def wrap[T](a:T) = Cons(a, Nil)

  def nil[T](l:List[T]) = l == Nil

  def foldL[T,U](e:U, l:List[T])(f:(T,U)=>U):U = l match{
    case Cons(x, xs) => f(x, (foldL(e, xs)(f)))
    case _ => e
  }
	     
  def mapL[T,U](l:List[T])(f:T=>U):List[U] = 
    foldL[T,List[U]](Nil, l)((x,y)=>Cons(f(x),y))

  def appendL[T](a:List[T], b:List[T]):List[T] = foldL(b,a)(Cons.apply)

  def concatL[T](a:List[List[T]]):List[T] = 
    foldL[List[T],List[T]](Nil,a)((x,y)=>foldL(y,x)(Cons.apply))

  def isort[T<%Ordered[T]](l:List[T]) = foldL[T, List[T]](Nil, l)(insert)

  def insert[T<%Ordered[T]](y:T, l:List[T]):List[T] = l match{
    case Cons(x, xs) => if(y<x) Cons(y, Cons(x,xs)) else Cons(x, insert(y, xs))
    case _ => wrap(y)
  }

  // 練習問題3.4
  // foldLで書くためにはf:(T, (List[T], List[T]))=>(List[T], List[T])
  def insert1[T<%Ordered[T]](y:T, l:List[T]):(List[T],List[T]) = 
    foldL[T, (List[T], Cons[T])]((Nil,wrap(y)), l){
      case (a, (b ,Cons(x, xs))) => if(x<a) (Cons(a, b), Cons(x, Cons(a,b))) else
	(Cons(a, b), Cons(a, Cons(x,xs)))
    }
	

  def isort2[T<%Ordered[T]](l:List[T]) = {
    def insert(y:T, l:List[T]): List[T] = insert1(y,l) match {
      case (_, ret) => ret
    }
    foldL[T, List[T]](Nil, l)(insert)    
  }

  // 練習問題3.5
  def isort3[T<%Ordered[T]](l:List[T]) = {
    def insert(y:T, l:List[T]): List[T] = 
      paraL[T, List[T]](wrap(y), l){
	case (a, (b, Nil)) => Cons(a, b)
	case (a, (b, Cons(x, xs))) => if(x<a) Cons(x, Cons(a,b)) else Cons(a, Cons(x,xs))
      }

    foldL[T, List[T]](Nil, l)(insert)    
  }

  def paraL[T,U](e:U, l:List[T])(f:(T,(List[T], U))=>U):U = l match{
    case Cons(x,xs) => f(x,(xs, paraL(e,xs)(f)))
    case _ => e
  }
}
