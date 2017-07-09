/**
  * Created by rajkumar on 7/9/17.
  */

object UnbalancedSetTest extends App {

  trait Ordering {
    type T
    def compare(x: T, y: T): Int
  }

  object IntOrdering extends Ordering {
    type  T = Int

    override def compare(x: Int, y: Int): Int = x - y
  }
  
  trait  SetSig {
    type Elem
    type Set
    def empty: Set
    def insert(e: Elem, s: Set): Set
    def member(e: Elem, s: Set): Boolean
  }

  abstract class UnbalancedSet extends SetSig {
    //type Elem = o.T
    //val Element: Ordering = o
    sealed trait  Set
    final case object Leaf extends Set
    final case class Branch(left: Set, elem: Elem, right: Set) extends Set

    val empty = Leaf


  }

  object S extends UnbalancedSet {

    type Elem = Int
    val Element: Ordering  = IntOrdering
    def member (x: Elem, s: Set): Boolean = s match {
      case Leaf => false
      case Branch(l, y, r) => if(IntOrdering.compare(x, y) < 0)
        member(x, l)
      else if(IntOrdering.compare(x, y) > 0)
        member(y, r)
      else
        true
    }

    override def insert(x: IntOrdering.T, s: Set): Set = s match {
      case Leaf => Branch(Leaf, x, Leaf)
      case Branch(l, y, r) => if(IntOrdering.compare(x, y) < 0)
        Branch(insert(x, l), y, r)
      else if(IntOrdering.compare(y, x) > 0)
        Branch(l, y, insert(x, r))
      else s
    }
  }

  println(S.insert(1, S.empty))
  //println(S.empty.ins)
}


