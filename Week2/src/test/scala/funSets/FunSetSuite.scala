package funSets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  test("Contains is Implemented"){
    assert (contains(x=> true,100))
  }

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  test("sigleton(1) contains 1"){
    new TestSets {
      assert(contains(s1,1), "Singleton 1")
      assert(!contains(s1,2), "Singleton 1 does not contain 2")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect is empty if no intersection between 2 sets") {
    new TestSets {
      val u = intersection(s1, s2)
      assert(!contains(u, 1), "intersect does not contain 1")
      assert(!contains(u, 2), "intersect does not contain 2")
    }
  }

  test("intersect contains elements in both sets") {
    new TestSets {
      val i = intersection(union(s1, s2), union(s2, s3))
      assert(!contains(i, 1), "intersect does not contain 1")
      assert(!contains(i, 3), "intersect does not contain 3")
      assert(contains(i, 2), "intersect contains 2")
    }
  }

  test("difference contains odd element of first sets only"){
    new TestSets {
      val d = diff(union(s1,s2),union(s2,s3))
      assert(contains(d,1), "difference contains 1")
      assert(!contains(d,2), "difference does not contain 2")
    }
  }

  test("filter contain elements that satisfies the given predicate"){
    new TestSets {
      val f = filter(union(union(s1,s2),s3),x=> x % 2 == 0)
      assert(!contains(f,1), "filter do not contain 1")
      assert(contains(f,2), "filter contain 2")
    }
  }

  test("forall some element does not meet condition") {
    new TestSets {
      val all = union(union(s1, s2), s3)
      assert(!forall(all, (x: Int) => x < 1), "1 is not smaller than 1")
    }
  }

  test("exists at least one element that meet condition") {
    new TestSets {
      val all = union(union(s1, s2), s3)
      assert(exists(all, (x: Int) => x < 2), "1 is smaller than 2")
      assert(exists(all, (x: Int) => x < 100), "all is smaller than 100")
    }
  }

  test("exists no element that meet condition") {
    new TestSets {
      val all = union(union(s1, s2), s3)
      assert(!exists(all, (x: Int) => x < 1), "no element is smaller than 1")
    }
  }

  test("map elements to new values") {
    new TestSets {
      val all = union(union(s1, s2), s3)
      val mapped = map(all, x => x * x)
      assert(contains(mapped, 1), " 1 = 1^2")
      assert(contains(mapped, 4), " 4 = 2^2")
      assert(contains(mapped, 9), " 9 = 3^2")
    }
  }

}


