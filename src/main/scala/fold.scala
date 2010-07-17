
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

  // １つめのfoldLが取る関数は(List[T],List[T])=>List[T]
  def concatL[T](a:List[List[T]]):List[T] = 
    foldL[List[T],List[T]](Nil,a)((x,y)=>foldL(y,x)(Cons.apply))
}
