package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val singleLeaf = Leaf('a', 1)
  }

  trait TestCharLists {
    val emptyList = List()
    val aList = List('a')
    val abcList = List('a', 'b', 'c')
    val a2b2c1List = List('a', 'b', 'a', 'b', 'c')
  }

  trait TestListTrees {
    val singletonList = List(Leaf('a', 3))
    val leafList = List(Leaf('r', 2), Leaf('b', 4))
    val someLeaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
  }

  test("weight on single element tree") {
    new TestTrees {
      assert(weight(singleLeaf) === 1)
    }
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a leaf") {
    new TestTrees {
      assert(chars(singleLeaf) == List('a'))
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("string2chars(\"\")") {
    assert(string2Chars("") === List())
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times char lists") {
    new TestCharLists {
      assert(times(emptyList) == List(), "empty")
      assert(times(aList) == List(('a', 1)))
    }
  }

  test("times for longer lists") {
    new TestCharLists {
      val abcTimes = times(abcList)
      assert(abcTimes.contains(('a', 1)))
      assert(abcTimes.contains(('b', 1)))
      assert(abcTimes.contains(('c', 1)))

      val a2b2c2Times = times(a2b2c1List)
      assert(a2b2c2Times.contains(('a', 2)))
      assert(a2b2c2Times.contains(('b', 2)))
      assert(a2b2c2Times.contains(('c', 1)))
    }
  }

  test("makeOrderedLeafList") {
    assert(makeOrderedLeafList(List()) === List())
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
    assert(makeOrderedLeafList(List(('a', 1), ('b', 2), ('c', 3))) === List(Leaf('a', 1), Leaf('b', 2), Leaf('c', 3)))
  }

  test("singleton") {
    new TestTrees {
      assert(singleton(List(singleLeaf)) === true)
      assert(singleton(List(t1, t2)) === false)
    }
  }

  test("combine a single leaf") {
    new TestTrees {
      assert(combine(List(singleLeaf)) === List(singleLeaf))
    }
  }

  test("combine list of two leaf") {
    new TestListTrees {
      assert(combine(leafList) === List(Fork(Leaf('r', 2), Leaf('b', 4), List('r', 'b'), 6)))
    }
  }

  test("combine of some leaf list") {
    new TestListTrees {
      assert(combine(someLeaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
    }
  }

  test("until on singleton") {
    new TestListTrees {
      assert(until(singleton, combine)(singletonList) === Leaf('a', 3))
    }
  }

  test("until list of two leaf") {
    new TestListTrees {
      assert(until(singleton, combine)(leafList) === Fork(Leaf('r', 2), Leaf('b', 4), List('r', 'b'), 6))
    }
  }

  test("until list of some leaf") {
    new TestListTrees {
      assert(until(singleton, combine)(someLeaflist) === Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7))
    }
  }

  test("Single element tree") {
    assert(createCodeTree(string2Chars("a")) === Leaf('a', 1))
  }

  test("decode secret") {
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a longer text should be identity") {
    new TestTrees {
      assert(decode(t2, encode(t2)("abdabdabdabdbadbabdbadbabdabdbabdbabdabdbabdbabd".toList)) === "abdabdabdabdbadbabdbadbabdabdbabdbabdabdbabdbabd".toList)
    }
  }

  test("decode and fast encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("encode some text with frenchCode") {
    assert(encode(frenchCode)("encoreuntextetressecret".toList) == List(1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1))
  }

  test("encode short text with frenchCode") {
    assert(decode(frenchCode, encode(frenchCode)("sr".toList)) == "sr".toList)
  }

  test("convert") {
    val tree = Fork(Fork(Leaf('a', 1), Leaf('b', 1), List('a', 'b'), 2), Leaf('d', 1), List('a', 'b', 'd'), 3)
    assert(convert(tree) == List(('a',List(0, 0)), ('b',List(0, 1)), ('d',List(1))))
  }
}
