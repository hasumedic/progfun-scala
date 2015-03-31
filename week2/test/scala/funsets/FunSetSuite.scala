package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 * - run the "test" command in the SBT console
 * - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   * - test
   * - ignore
   * - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   * val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val negativeSingleton = singletonSet(-1)
    val moreThanOneElementSet = union(union(s1, s2), s3)
    val positiveIntegers = (x: Int) => x > 0 && x < 1000
    val s_even = (x: Int) => x % 2 == 0
    val s_allnumbers = (x: Int) => x >= -bound && x <= bound

  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Triangulation")
      assert(contains(s3, 3), "Triangulation")
      assert(!contains(s1, 2), "Not contains")
      assert(!contains(s2, 10), "Not contains triangulation")
      assert(contains(negativeSingleton, -1), "negative singleton")
      assert(!contains(negativeSingleton, -2), "negative singleton not contains")
    }
  }

  test("Union") {
    new TestSets {
      val unionSet = union(s1, s2)
      assert(contains(unionSet, 1), "Union 1")
      assert(contains(unionSet, 2), "Union 2")
      assert(!contains(unionSet, 3), "Union 3")

      val anotherUnion = union(unionSet, negativeSingleton)
      assert(contains(anotherUnion, 1))
      assert(contains(anotherUnion, 2))
      assert(contains(anotherUnion, -1))
      assert(!contains(anotherUnion, 3))
    }
  }

  test("Intersect") {
    new TestSets {
      val intersectSet = intersect(s1, s2)
      assert(!contains(intersectSet, 1), "Intersect 1")
      assert(!contains(intersectSet, 2), "Intersect 2")
      assert(!contains(intersectSet, 3), "Intersect 3")

      val setOne = singletonSet(1)
      val secondIntersectSet = intersect(moreThanOneElementSet, setOne)
      assert(contains(secondIntersectSet, 1))
      assert(!contains(secondIntersectSet, 2))
      assert(!contains(secondIntersectSet, 3))
    }
  }

  test("diff") {
    new TestSets {
      val diffOnSameSet = diff(s1, s1)
      assert(!contains(diffOnSameSet, 1))

      val diffOnSingleSets = diff(s1, s2)
      assert(contains(diffOnSingleSets, 1))
      assert(!contains(diffOnSingleSets, 2))

      val diffOnMultiSets = diff(moreThanOneElementSet, s1)
      assert(contains(diffOnMultiSets, 2), "contains 2")
      assert(contains(diffOnMultiSets, 3), "contains 3")
      assert(!contains(diffOnMultiSets, 1), "not contains 1")
    }
  }

  test("filter") {
    new TestSets {
      val negativeFilter = filter(s1, (x: Int) => x < 0)
      assert(!contains(negativeFilter, 1))

      val positiveFilter = filter(s1, (x: Int) => x > 0)
      assert(contains(positiveFilter, 1))

      val oddFilter = filter(moreThanOneElementSet, (x: Int) => x % 2 == 1)
      assert(contains(oddFilter, 1))
      assert(!contains(oddFilter, 2))
      assert(contains(oddFilter, 3))

      val evenSet = (x: Int) => x % 2 == 0
      val evenFilter = filter(evenSet, (y: Int) => y % 2 == 0)
      assert(contains(evenFilter, 100), "contains even")
      assert(contains(evenFilter, 0), "contains 0")
      assert(!contains(evenFilter, 1), "not contains odd")
    }
  }

  test("forall") {
    new TestSets {
      assert(forall(positiveIntegers, (x: Int) => x > 0))
      assert(!forall(positiveIntegers, (x: Int) => x < 0))
      assert(forall(negativeSingleton, (x: Int) => x < 0))
    }
  }

  test("exists") {
    new TestSets {
      assert(exists(s1, (x: Int) => x == 1))
      assert(!exists(s1, (x: Int) => x == 2))
      assert(!exists(positiveIntegers, (x: Int) => x < 0))
      assert(exists(moreThanOneElementSet, (x: Int) => x == 1))
    }

    new TestSets {
      assert(exists(s1, (x: Int) => (x == 1)), "exists a 1 in set 1")
      assert(!exists(s1, (x: Int) => (x == 2)), "!exists a 2 in set 1")

      val s_union = filter(s_allnumbers, x => (x % 2 == 0) || x == 3)
      assert(exists(s_union, x => x % 2 == 1), "even and 3")
    }
  }

  test("map") {
    new TestSets {
      val incrementedSet = map(s1, (x: Int) => x + 1)
      assert(contains(incrementedSet, 2))

      val subtractedSet = map(moreThanOneElementSet, (x: Int) => x - 3)
      assert(contains(subtractedSet, 0))
      assert(contains(subtractedSet, -1))
      assert(contains(subtractedSet, -2))
      assert(!contains(subtractedSet, -3))
    }

    new TestSets {
      assert(!contains(map(s_even, (x: Int) => x + 1), 100))
      assert(contains(map(s_even, (x: Int) => x + 1), 101))
    }
  }
}
