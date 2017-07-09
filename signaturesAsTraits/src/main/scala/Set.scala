/**
  * Created by rajkumar on 7/9/17.
  */



sealed abstract class Set[+A] {
  import Set._

  def insert[B >: A](x: B) (implicit  o: Ordering[B]): Set[B] = this match {
    case Leaf => Branch(Leaf, x, Leaf)
    case b@ Branch(l, y, r) => if(o.compare(x, y) < 0)
      Branch(l.insert(x), y, r)
    else if(o.compare(x, y) > 0)
      Branch(l, y, r.insert(x))
    else b
  }

  def member[B >: A](x: B) (implicit o: Ordering[B]): Boolean = this match {
    case Leaf => false
    case Branch(l, y, r) => if(o.compare(x, y) > 0) l.member(x)
    else if(o.compare(x,y) < 0) r.member(x)
    else  return true
  }

  def isEmpty: Boolean

}

object Set {

  def empty[A]: Set[A] = Leaf

  case object Leaf extends Set[Nothing] {
    val isEmpty = true
  }

  final case class Branch[+A] (left: Set[A], elem:A, right: Set[A]) extends Set[A] {
    val isEmpty = false
  }
}
