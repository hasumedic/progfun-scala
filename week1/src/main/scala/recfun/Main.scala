package recfun

import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c > r) throw new IndexOutOfBoundsException
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1);
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceLoop(chars: List[Char], parCount: Int): Boolean = {
      if (chars.isEmpty || parCount < 0) parCount == 0
      else if (chars.head == '(') balanceLoop(chars.tail, parCount + 1)
      else if (chars.head == ')') balanceLoop(chars.tail, parCount - 1)
      else balanceLoop(chars.tail, parCount)
    }

    balanceLoop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def doCountChange(money: Int, coins: List[Int]): Int = {
      if (money == 0 || coins.isEmpty || money - coins.head < 0) 0
      else if (money - coins.head == 0) 1
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }

    doCountChange(money, coins.sortWith(_ < _).distinct)
  }
}


