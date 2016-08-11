import cats.Foldable
import org.scalatest.{Matchers, WordSpec}

/**
  * Created by ronnie on 2016. 8. 11..
  */
class FoldableSpec extends WordSpec with Matchers{
  
  "fold" in {
    import cats.std.list.listInstance
    
    // sum-case1
    val fold1: Int = Foldable[List].foldLeft(List[Int](1, 2, 3, 4, 5), 0)(_ + _)
    println("fold1 = " + fold1)
    
    // sum-case2
    val fold2 = Foldable[List].foldLeft(List(1, 2, 3, 4, 5), 0) {
      case (acc, x) => acc + x
    }
    println("fold2 = " + fold2)

    // filter
    val fold3 = Foldable[List].foldLeft(List(1, 2, 3, 4, 5), List[Int]()) {
      case (acc, x) => if (x % 2 == 0) acc ::: List(x) else acc
    }
    println("fold3 = " + fold3)
    
    // map
    def myMap(list : List[Int])(f : Int => Int) : List[Int] = {
      Foldable[List].foldLeft(list, List[Int]()) {
        case (acc, x) => acc ::: List(f(x)) 
      }
    }
    val fold4: List[Int] = myMap(List(1, 2, 3, 4, 5))(x => x + 10)
    println("fold4 = " + fold4)
  }
}
