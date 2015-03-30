package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange

  test("countChange: change of 0") {
    assert(countChange(0, List(1)) === 0)
  }

  test("countChange: change without coins") {
    assert(countChange(5, List()) === 0)
  }

  test("countChange: coin too big") {
    assert(countChange(5, List(6)) === 0)
  }

  test("countChange: same amount and coin") {
    assert(countChange(5, List(5)) === 1)
  }

  test("countChange: duplicates in list of coins") {
    assert(countChange(2, List(1, 1)) === 1)
  }

  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }
}
