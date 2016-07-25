import org.scalatest.{Matchers, WordSpec}

/**
  * Created by ronnie on 2016. 7. 20..
  */
class MonoidSpec extends WordSpec with Matchers {

  "Monoid" should {
    "ex" in {

      import cats._
      import cats.std.all._
      import cats.syntax.all._

      Monoid[String].empty shouldBe ""
      Monoid[String].combineAll(List("a", "b", "c")) shouldBe "abc"
      Monoid[String].combineAll(List()) shouldBe ""

      Monoid[Map[String, Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3))) shouldBe Map("a" -> 4, "b" -> 2)
      Monoid[Map[String, Int]].combineAll(List()) shouldBe Map()

      val l = List(1, 2, 3, 4, 5)
      l.foldMap(identity) shouldBe 15
      l.foldMap(i => i.toString) shouldBe "12345"

      val ll = List(1, 2, 3, 4, 5)
      ll.foldMap(j => (j, j.toString)) shouldBe(15, "12345")
    }

    "produce a tuple" in {
      import cats._
      import cats.implicits._

      implicit def monoidTuple[A: Monoid, B: Monoid]: Monoid[(A, B)] = new Monoid[(A, B)] {
        def combine(x: (A, B), y: (A, B)): (A, B) = {
          val (xa, xb) = x
          val (ya, yb) = y
          (Monoid[A].combine(xa, ya), Monoid[B].combine(xb, yb))
        }

        def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)
      }

      Monoid[(Int, Int)].combine((1, 2), (3, 4)) shouldBe (4, 6)
    }
  }
}
