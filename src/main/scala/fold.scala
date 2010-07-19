
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

  def foldLp[T,U](l:List[T])(f:Option[(T,U)]=>U):U = l match{
    case Cons(x,xs) => f(Some(x,foldLp(xs)(f)))
    case Nil => f(None)
  }

  // 練習問題3.8 foldLをfoldLpで書く
  def foldL2[T,U](e:U, l:List[T])(f:(T,U)=>U):U = foldLp(l){ o:Option[(T,U)] => o match{
    case Some((x,y)) => f(x,y)
    case _ => e
  }}

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
  def foldLargs[T,U](e:U)(f:(T,U)=>U):Option[(T,U)]=>U = (_:Option[(T,U)]) match{
      case Some((x,y)) => f(x,y)
      case _ => e
    }
  
  // foldLpとfoldLargsでfoldLが書けるはず
  def foldL3[T,U](e:U, l:List[T])(f:(T,U)=>U):U = foldLp(l){foldLargs(e)(f)}

  def delmin[T<%Ordered[T]](l:List[T]):Option[(T,List[T])] = l match{
    case xs:Cons[T] => 
      val y = minimumL(xs)
      Some(y, deleteL(y,xs))
    case _ => None
  }
  

  def min[T<%Ordered[T]](a:T, b:T) = if(a<b)a else b

  def minimumL[T<%Ordered[T]](l:Cons[T]):T = l match {case Cons(x,xs) => foldL(x,xs)(min)}

  def deleteL[T](y:T,l:List[T]):List[T] = l match {
    case Cons(x,xs) => if (y==x) xs else Cons(x, deleteL(y, xs))
    case _ => Nil
  }

  def ssort[T<%Ordered[T]](l:List[T]):List[T] = unfoldLp[T,List[T]](l)(delmin)
}
