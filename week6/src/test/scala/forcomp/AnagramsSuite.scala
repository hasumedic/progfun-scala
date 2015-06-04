package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: empty string") {
    assert(wordOccurrences("") === List())
  }

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("wordOccurrences: Spec1al-Ch4rs") {
    assert(wordOccurrences("Spec1al-Ch4rs") === List(('a', 1), ('c', 2), ('e', 1), ('h', 1), ('l', 1), ('p', 1), ('r', 1), ('s', 2)))
  }


  test("sentenceOccurrences: empty strings") {
    assert(sentenceOccurrences(List("", "")) === List())
  }

  test("sentenceOccurrences: Capital Letters") {
    assert(sentenceOccurrences(List("Capital", "Letters")) === List(('a', 2), ('c', 1), ('e', 2), ('i', 1), ('l', 2), ('p', 1), ('r', 1), ('s', 1), ('t', 3)))
  }

  test("sentenceOccurrences: Spec1al Ch4rs") {
    assert(sentenceOccurrences(List("Spec1al", "Ch4rs")) === List(('a', 1), ('c', 2), ('e', 1), ('h', 1), ('l', 1), ('p', 1), ('r', 1), ('s', 2)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }


  test("dictionaryByOccurrences.get: xxxxx") {
    assert(dictionaryByOccurrences.get(List(('x', 5))).map(_.toSet) === None)
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }


  test("word anagrams: xxxxx") {
    assert(wordAnagrams("xxxxx").toSet === Set())
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }


  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = wordOccurrences("aabb")
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: aabbcc") {
    val aabbcc = wordOccurrences("aabbcc")
    val aabbcccomb = List(
      List(),
      List(('c', 1)),
      List(('c', 2)),
      List(('b', 1)),
      List(('b', 1), ('c', 1)),
      List(('b', 1), ('c', 2)),
      List(('b', 2)),
      List(('b', 2), ('c', 1)),
      List(('b', 2), ('c', 2)),
      List(('a', 1)),
      List(('a', 1), ('c', 1)),
      List(('a', 1), ('c', 2)),
      List(('a', 1), ('b', 1)),
      List(('a', 1), ('b', 1), ('c', 1)),
      List(('a', 1), ('b', 1), ('c', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 1), ('b', 2), ('c', 1)),
      List(('a', 1), ('b', 2), ('c', 2)),
      List(('a', 2)),
      List(('a', 2), ('c', 1)),
      List(('a', 2), ('c', 2)),
      List(('a', 2), ('b', 1)),
      List(('a', 2), ('b', 1), ('c', 1)),
      List(('a', 2), ('b', 1), ('c', 2)),
      List(('a', 2), ('b', 2)),
      List(('a', 2), ('b', 2), ('c', 1)),
      List(('a', 2), ('b', 2), ('c', 2))
    )
    assert(combinations(aabbcc).toSet === aabbcccomb.toSet)
  }


  test("subtract: [] - []") {
    val empty = wordOccurrences("")
    val empty2 = wordOccurrences("")
    val expected = wordOccurrences("")
    assert(subtract(empty, empty2) === expected)
  }

  test("subtract: lard - []") {
    val empty = wordOccurrences("")
    val lard = wordOccurrences("lard")
    val expected = wordOccurrences("lard")
    assert(subtract(lard, empty) === expected)
  }

  test("subtract: lard - r") {
    val lard = wordOccurrences("lard")
    val r = wordOccurrences("r")
    val lad = wordOccurrences("lad")
    assert(subtract(lard, r) === lad)
  }

  test("subtract: bababa - ab") {
    val bababa = wordOccurrences("bababa")
    val ab = wordOccurrences("ab")
    val expected = wordOccurrences("aabb")
    assert(subtract(bababa, ab) === expected)
  }


  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }
}
