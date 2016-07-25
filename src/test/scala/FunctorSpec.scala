import org.scalatest.{Matchers, WordSpec}

/**
  * Created by ronnie on 2016. 7. 25..
  */
class FunctorSpec extends WordSpec with Matchers {
  
  import cats._
  
  "functor exercises" should {
    
    "instance" in {
      implicit val optionInstance = new Functor[Option] {
        override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = {
          fa match {
            case Some(x) => Some(f(x))
            case None => None
          }
        }
      }
      Functor[Option].map(Option("Hello"))(_.length) shouldBe Some(5)
      Functor[Option].map(None: Option[String])(_.length) shouldBe None

      val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
      lenOption(Some("Hello")) shouldBe Option(5)
    
      implicit val listInstance = new Functor[List] {
        override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa.map(f(_))
        
      }
      
      Functor[List].map(List("qwer", "adsfg"))(_.length) shouldBe List(4, 5)

      val source: List[String] = List("Cats", "is", "awesome")
      val product: Map[String, Int] = Functor[List].fproduct(source)(_.length).toMap
      product.get("Cats").getOrElse(0) shouldBe 4
      product.get("is").getOrElse(0) shouldBe 2
      product.get("awesome").getOrElse(0) shouldBe 7

      val listOpt = Functor[List] compose Functor[Option]
      listOpt.map(List(Some(1), None, Some(3)))(_ + 1) shouldBe  List(Some(2), None, Some(4))
    }

    "my foo instance" in {
      import cats.syntax.functor._
      case class Foo[T](a: T)
      implicit val fooInstance = new Functor[Foo] {
        override def map[A, B](fa: Foo[A])(f: (A) => B): Foo[B] = Foo(f(fa.a))
      }

      val foo = Foo[Int](1)
      foo.map(_ + 1) shouldBe Functor[Foo].map(foo)(_ + 1)
    }
  }
}
