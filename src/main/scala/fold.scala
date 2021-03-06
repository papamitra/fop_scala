
object Fold{
  sealed trait List[+T]

  object Nil extends List[Nothing]

  case class Cons[+T](val a:T, l:List[T]) extends List[T]

  def wrap[T](a:T) = Cons(a, Nil)

  def nil[T](l:List[T]) = l == Nil

  def foldL[T,U](f:(T,U)=>U,e:U, l:List[T]):U = l match{
    case Cons(x, xs) => f(x, (foldL(f, e, xs)))
    case _ => e
  }

  def foldLp[T,U](f:Option[(T,U)]=>U, l:List[T]):U = l match{
    case Cons(x,xs) => f(Some(x,foldLp(f,xs)))
    case Nil => f(None)
  }

  // 練習問題3.8 foldLをfoldLpで書く
  def foldL2[T,U](f:(T,U)=>U,e:U, l:List[T]):U = foldLp({ o:Option[(T,U)] => o match{
    case Some((x,y)) => f(x,y)
    case _ => e
  }}, l)

  def mapL[T,U](l:List[T])(f:T=>U):List[U] = 
    foldL[T,List[U]]((x,y)=>Cons(f(x),y),Nil, l)

  def appendL[T](a:List[T], b:List[T]):List[T] = foldL[T,List[T]](Cons.apply,b,a)

  def concatL[T](a:List[List[T]]):List[T] = 
    foldL[List[T],List[T]]((x,y)=>foldL[T,List[T]](Cons.apply,y,x), Nil,a)

  def isort[T<%Ordered[T]](l:List[T]) = foldL[T, List[T]](insert, Nil, l)

  def insert[T<%Ordered[T]](y:T, l:List[T]):List[T] = l match{
    case Cons(x, xs) => if(y<x) Cons(y, Cons(x,xs)) else Cons(x, insert(y, xs))
    case _ => wrap(y)
  }

  // 練習問題3.4
  // foldLで書くためにはf:(T, (List[T], List[T]))=>(List[T], List[T])
  def insert1[T<%Ordered[T]](y:T, l:List[T]):(List[T],List[T]) = 
    foldL[T, (List[T], Cons[T])]({
      case (a, (b ,Cons(x, xs))) => if(x<a) (Cons(a, b), Cons(x, Cons(a,b))) else
	(Cons(a, b), Cons(a, Cons(x,xs)))
    }, (Nil,wrap(y)), l)
	

  def isort2[T<%Ordered[T]](l:List[T]) = {
    def insert(y:T, l:List[T]): List[T] = insert1(y,l) match {
      case (_, ret) => ret
    }
    foldL[T, List[T]](insert,Nil, l)
  }

  // 練習問題3.5
  def isort3[T<%Ordered[T]](l:List[T]) = {
    def insert(y:T, l:List[T]): List[T] = 
      paraL[T, List[T]](wrap(y), l){
	case (a, (b, Nil)) => Cons(a, b)
	case (a, (b, Cons(x, xs))) => if(x<a) Cons(x, Cons(a,b)) else Cons(a, Cons(x,xs))
      }

    foldL[T, List[T]](insert,Nil, l)
  }

  def paraL[T,U](e:U, l:List[T])(f:(T,(List[T], U))=>U):U = l match{
    case Cons(x,xs) => f(x,(xs, paraL(e,xs)(f)))
    case _ => e
  }

  // bがリストだとしてunfoldL(nil, head, tail, b)とするともとのリストbができる.
  def unfoldL[T, U](p:U=>Boolean, f:U=>T, g:U=>U, b:U):List[T] =
    if (p(b)) Nil else Cons(f(b), unfoldL(p,f,g,g(b)))

  def unfoldLp[T,U](u:U)(f:U=>Option[(T,U)]):List[T] = f(u) match{
    case None => Nil
    case Some((x,v)) => Cons(x, (unfoldLp(v)(f)))
  }
   
  // 練習問題3.6　unfoldL'をunfoldLで書く
  def unfoldLp2[T,U](u:U)(f:U=>Option[(T,U)]):List[T] = 
    unfoldL(f andThen ((_:Option[(T,U)]).!=(None)), // p:U=>Boolean
	    f andThen { case Some((x,y)) => x}, // f:U=>T
	    f andThen { case Some((x,y)) => y}, // g:U=>U
	    u)

  // unfoldLをunfoldL'で書く
  def unfoldL2[T, U](p:U=>Boolean, f:U=>T, g:U=>U, b:U):List[T] =
    unfoldLp(b)( (x:U) => if(!p(x)) None else Some(f(x), g(x)))

  
  // 練習問題3.9
  def foldLargs[T,U](f:(T,U)=>U, e:U):Option[(T,U)]=>U = (_:Option[(T,U)]) match{
      case Some((x,y)) => f(x,y)
      case _ => e
  }
  
  // foldLpとfoldLargsでfoldLが書けるはず
  def foldL3[T,U](f:(T,U)=>U,e:U, l:List[T]):U = foldLp(foldLargs(f,e),l)

  def delmin[T<%Ordered[T]](l:List[T]):Option[(T,List[T])] = l match{
    case xs:Cons[T] => 
      val y = minimumL(xs)
      Some(y, deleteL(y,xs))
    case _ => None
  }
  

  def min[T<%Ordered[T]](a:T, b:T) = if(a<b)a else b

  def minimumL[T<%Ordered[T]](l:Cons[T]):T = l match {case Cons(x,xs) => foldL(min[T],x,xs)}

  def deleteL[T](y:T,l:List[T]):List[T] = l match {
    case Cons(x,xs) => if (y==x) xs else Cons(x, deleteL(y, xs))
    case _ => Nil
  }

  def ssort[T<%Ordered[T]](l:List[T]):List[T] = unfoldLp[T,List[T]](l)(delmin)

  // 練習問題3.10
  def deleteL2[T](y:T,l:List[T]):List[T] = paraL[T,List[T]](Nil, l){
    case (a, (b, xs)) => if(a == y) b else Cons(a,xs)
  }
  
  // 練習問題3.11 delminをparaLで
  // U => (y:T, a:List[T], b:List[T])はyが最小値, aがすでに最小値を除いたリスト
  def delmin2[T<%Ordered[T]](l:List[T]):Option[(T,List[T])] = paraL[T, Option[(T, List[T])]](None, l){
    case (x, (xs, Some((y, a)) )) => if(x <= y) Some((x, xs)) else Some((y, Cons(x,a)))
    case (x, (xs, None)) => Some((x, xs))
  }

  def bubble[T<%Ordered[T]](l:List[T]):Option[(T,List[T])] ={
    def step(x:T, xs:Option[(T,List[T])]) = xs match{
      case Some((y,ys)) => if(x<y) Some((x,Cons(y,ys))) else Some((y,Cons(x,ys)))
      case None => Some(x, Nil)
    }
    
    foldL[T, Option[(T,List[T])]](step,None, l)
  }
	   
  def bsort[T<%Ordered[T]](l:List[T]):List[T] = unfoldLp[T,List[T]](l)(bubble)

  // 練習問題3.12 bubble2の定義
  def bubble2[T<%Ordered[T]](l:List[T]):List[T] = {
    def step(x:T, xs:List[T]) = xs match{
      case Cons(y,ys) => if(x<y) Cons(x, Cons(y,ys)) else Cons(y, Cons(x,ys))
      case Nil => Cons(x,Nil)
    }
    foldL[T, List[T]](step,Nil, l)
  }

  // bubble2を使ってbsortを定義。bubble2で作ったリストを結局展開する必要があるので
  // bubbleを使った定義の方が自然.
  def bsort2[T<%Ordered[T]](l:List[T]):List[T] = 
    unfoldL[T,List[T]](nil,
		       bubble2(_) match{case Cons(x,xs)=>x},
		       bubble2(_) match{case Cons(x,xs) => xs}, l)

  // 練習問題3.13
  // 挿入を受けるリストと挿入される要素にMaybeをかぶせたものの対 (Option[T], List[T])
  def insert4[T<%Ordered[T]](y:T, l:List[T]):List[T] = unfoldLp[T,(Option[T],List[T])]((Some(y),l)){
    case (None, l) => l match {
			case Cons(x,xs) => Some((x,(None, xs)))
			case _ => None
		      }
    case (Some(y), l) => l match{
			   case Cons(x,xs) => if(y<x) Some(y,(None, Cons(x,xs))) else Some(x,(Some(y),xs))
			   case _ => Some(y,(None,Nil))
			  }
  }

  def isort4[T<%Ordered[T]](l:List[T]) = {
    foldL[T, List[T]](insert4, Nil, l)
  }

  // 練習問題3.14
  def apoLp[T,U](a:U)(f:U=>Option[(T,Either[U,List[T]])]):List[T] = f(a) match{
    case None => Nil
    case Some((x, Left(v))) => Cons(x, (apoLp(v)(f)))
    case Some((x, Right(xs))) => Cons(x, xs)
  }

  def insert5[T<%Ordered[T]](y:T, l:List[T]):List[T] = apoLp[T,List[T]](l){
    case Cons(x,xs) => if(y<x) Some(y, Right(Cons(x,xs))) else Some(x, Left(xs))
    case _ => Some(y, Right(Nil))
  }

  def isort5[T<%Ordered[T]](l:List[T]) = {
    foldL[T, List[T]](insert5, Nil, l)
  }

  def hyloL[T,U](f:(T,U)=>U, e:U, p:U=>Boolean, g:U=>T, h:U=>U, b:U):U = 
    ({foldL[T,U](f,e,_:List[T])} compose {unfoldL(p,g,h,_:U)})(b)

  def fact(n:Int):Int = hyloL[Int,Int]({_*_}, 1, _ == 0, {x=>x}, _ -1, n)

  // 練習問題3.15
  
}
