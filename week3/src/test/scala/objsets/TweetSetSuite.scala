package objsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val emptySet = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val duplicateSet2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val duplicateRetweets = emptySet.incl(new Tweet("a", "a", 5)).incl(new Tweet("b", "b", 5)).incl(new Tweet("c", "c", 5))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("filter: 30 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 30)) === 0)
    }
  }

  test("union: two empty sets") {
    new TestSets {
      assert(size(emptySet.union(emptySet)) === 0)
    }
  }

  test("union: set4c and set4d. Combined sets") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("union: with duplicates") {
    new TestSets {
      assert(size(set2.union(duplicateSet2)) === 1)
    }
  }

  test("mostRetweeted: on empty set") {
    new TestSets {
      intercept[NoSuchElementException] {
        emptySet.mostRetweeted
      }
    }
  }

  test("mostRetweeted: on single element set") {
    new TestSets {
      val tweet = set2.mostRetweeted
      assert(tweet.retweets == 20)
    }
  }

  test("mostRetweeted: on multiple elements with same number of retweets") {
    new TestSets {
      val tweet = duplicateRetweets.mostRetweeted
      assert(tweet.retweets == 5)
    }
  }

  test("mostRetweeted: on multiple elements with different number of retweets") {
    new TestSets {
      val tweet = set5.mostRetweeted
      assert(tweet.user === "a" || tweet.user === "b")
    }
  }

  test("descendingByRetweet: empty set") {
    new TestSets {
      val resultList = emptySet.descendingByRetweet
      assert(resultList.isEmpty)
    }
  }

  test("descendingByRetweet: single element set") {
    new TestSets {
      val resultList = set2.descendingByRetweet
      assert(!resultList.isEmpty)
      assert(resultList.head.retweets === 20)
    }
  }

  test("descendingByRetweet: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }
}
