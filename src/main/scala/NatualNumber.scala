
object NaturalNumber{
  sealed trait Nat
  object Zero extends Nat
  
  case class Succ(n:Nat) extends Nat

  def foldN[T](z:T, s:T=>T, n:Nat):T = n match{
    case Succ(n) => s(foldN(z,s,n))
    case Zero => z
  }

  def iter[T](n:Nat, f:T=>T):T=>T = foldN(_,f,n)

  implicit def num2Nat(n:Int):Nat=
    ((Zero:Nat) /: (1 to n)){(x,y)=>Succ(x)}

  def foldNp[T](f:Option[T]=>T, n:Nat):T = n match{
    case Succ(x) => f(Some(foldNp(f, x)))
  }

  // 練習問題3.16
  def foldN2[T](z:T, s:T=>T, n:Nat):T =
    foldNp({o:Option[T] => o match{
      case Some(x) => s(x)
      case None => z
    }}, n)


  def addN(x:Nat, y:Nat) = foldN(x, Succ, y)

  def mulN(x:Nat, y:Nat) = foldN(Zero, addN(_:Nat, y), x)

  def powN(x:Nat, y:Nat) = foldN(Succ(Zero), mulN(_:Nat, x), y)

}
