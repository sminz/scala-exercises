import org.scalatest.{Matchers, WordSpec}

/**
  * Created by ronnie on 2016. 8. 9..
  */
class MonadSpec extends WordSpec with Matchers {

  import cats._
  
  "foo monad instance" in {
    case class Foo[T](a: T)
    
    val fooMonadInstance = new Monad[Foo] {
      
      override def pure[A](x: A): Foo[A] = Foo[A](x)

      override def flatMap[A, B](fa: Foo[A])(f: (A) => Foo[B]): Foo[B] = f(fa.a)
    }

    fooMonadInstance.flatMap(Foo[Int](10))(x => Foo(x.toString)) shouldBe Foo[String]("10")
  }
  
  "option monad instance" in {
    val optionMonadInstance = new Monad[Option] {
      
      override def pure[A](x: A): Option[A] = Option(x)

      override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = {
        fa match {
          case Some(a) => f(a)
          case None => None
        }
      }
    }
    optionMonadInstance.flatMap(Option[Int](10))(x => Option(x)) shouldBe Option[Int](10)
  }
}
