import org.scalatest.{Matchers, WordSpec}

/**
  * Created by ronnie on 2016. 7. 20..
  */
class SemigroupSpec extends WordSpec with Matchers {

  import cats.Semigroup
  "Semigroup" should {
    
    "instance for int" in {
      import cats.implicits._
      
      Semigroup[Int].combine(1, 2) shouldBe 3
      Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
      Semigroup[Option[Int]].combine(Option(1), Option(2)) shouldBe Option(3)
      Semigroup[Option[Int]].combine(Option(1), None) shouldBe Option(1)
      Semigroup[Int => Int].combine({ (x: Int) ⇒ x + 1 }, { (x: Int) ⇒ x * 10 }).apply(6) shouldBe 67
    }
    
    "merge map" in {
      import cats.implicits._

      val aMap = Map("foo" -> Map("bar" -> 5))
      val anotherMap = Map("foo" -> Map("bar" -> 6))
      val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)

      combinedMap.get("foo") shouldBe Some(Map("bar" -> 11))
    }
    
    "inline syntax" in {
      import cats.implicits._

      val one: Option[Int] = Option(1)
      val two: Option[Int] = Option(2)
      val n: Option[Int] = None

      one |+| two shouldBe Some(3)
      n |+| two shouldBe Some(2)
      n |+| n shouldBe None
      two |+| n shouldBe Some(2)
    }
  }
}
