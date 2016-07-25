import org.scalatest.{Matchers, WordSpec}

/**
  * Created by ronnie on 2016. 7. 20..
  */
class MySemiSpec extends WordSpec with Matchers {
  
  "나의 세미 그룹을 만들어 보겠습니다." should {
    "그럼" in {
      import cats.Semigroup
      case class Foo(a: Int, b: String)
      implicit val fooSemigroup = new Semigroup[Foo] {
        override def combine(x: Foo, y: Foo): Foo =  Foo(x.a + y.a, x.b + y.b)
      }
      
      val foo1 = Foo(1, "a")
      val foo2 = Foo(2, "b")
      Semigroup[Foo].combine(foo1, foo2) shouldBe Foo(3, "ab")
      
      import cats.implicits._
      
      foo1 |+| foo2 shouldBe Foo(3, "ab")
    }
  }
}