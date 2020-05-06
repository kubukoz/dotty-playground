import scala.deriving._
import scala.compiletime.{erasedValue, summonInline}

trait Eq[T]:
  def (a: T).eqv(b: T): Boolean

object Eq:
  given Eq[Int]:
    def (a: Int).eqv(b: Int): Boolean = a == b

  inline def derived[T](using m: Mirror.Of[T]): Eq[T] = 
    val elems = allOf[m.MirroredElemTypes]
    inline m match {
      case s: Mirror.SumOf[T] => eqSum(s, elems)
      case p: Mirror.ProductOf[T] => eqProduct(p, elems)
    }

  inline def allOf[T <: Tuple]: List[Eq[_]] = inline erasedValue[T] match
    case _: Unit => Nil
    case _: (t *: ts) => summonInline[Eq[t]] :: allOf[ts]

  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  def check(eqv: Eq[_])(x: Any, y: Any): Boolean =
    eqv.asInstanceOf[Eq[Any]].eqv(x)(y)

  def eqProduct[T](p: Mirror.ProductOf[T], elems: List[Eq[_]]): Eq[T] =
    new Eq[T]:
      def (x: T).eqv(y: T): Boolean =
        iterator(x).zip(iterator(y)).zip(elems.iterator).forall {
          case ((x, y), elem) => check(elem)(x, y)
        }

  def eqSum[T](s: Mirror.SumOf[T], elems: List[Eq[_]]): Eq[T] =
    new Eq[T] {
      def (x: T).eqv(y: T): Boolean = 
        val ordx = s.ordinal(x)
        s.ordinal(y) == ordx && check(elems(ordx))(x, y)
    }
/* 
  inline given derived[T](using m: Mirror.Of[T]) as Eq[T] = {
    val elemInstances = summonAll[m.MirroredElemTypes]
    inline m match {
      case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
    }
  }
}
*/

enum Tree[T] derives Eq:
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(elem: T)

object Main:


  def main(args: Array[String]): Unit =
    println("Hello world!")
    println(msg)
    println(
      Tree.Branch(
        Tree.Leaf(1),
        Tree.Branch(Tree.Leaf(2), Tree.Leaf(3))
      ).eqv(Tree.Branch(
        Tree.Leaf(1),
        Tree.Leaf(2)
      )))


  def msg = "I was compiled by dotty :)"

