
/**
  * Created by rajkumar on 7/9/17.
  */
object SetModuleTest extends App {

  trait  SetModule {
    type Elem
    type Set <: SetSig


    trait  SetSig {
      def insert(e: Elem): Set
      def member(e: Elem): Boolean
    }

    def empty: Set
  }


  trait Ordering {
    type T
    def compare(x: T, y: T): Int
  }

  object IntOrdering extends Ordering {
    type  T = Int

    override def compare(x: Int, y: Int): Int = x - y
  }

  abstract class UnbalancedSet extends SetModule {
    val Element: Ordering
    type Elem = Element.T

    sealed abstract class Set extends SetSig {
      def member(x: Elem): Boolean = this match {
        case Leaf => false
        case Branch(l, y, r) =>
          if (Element.compare(x, y) < 0)
            l.member(x)
          else if (Element.compare(x, y) > 0)
            r.member(x)
          else
            true
      }

      def insert(x: Elem): Set = this match {
        case Leaf => Branch(Leaf, x, Leaf)
        case Branch(l, y, r) =>
          if (Element.compare(x, y) < 0)
            Branch(l.insert(x), y, r)
          else if (Element.compare(x, y) > 0)
            Branch(l, y, r.insert(x))
          else
            this
      }
    }
    case object Leaf extends Set
    case class Branch(left: Set, elem: Elem, right: Set) extends Set

    val empty = Leaf
  }


}
