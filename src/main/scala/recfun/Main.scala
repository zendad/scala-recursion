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
  def pascal(c: Int, r: Int): Int = if(c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def bal(left: Int, c: List[Char]): Boolean = {
      if (c.isEmpty) left == 0 else{
        val thisParan = c.head
        if (thisParan == '(') bal(left + 1, c.tail)
        else if (thisParan == ')')if (left - 1 < 0) false else bal(left - 1, c.tail)
        else bal(left, c.tail)
      }
    }
    bal(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(moneyLeft: Int, c: List[Int]): Int = {
      if (moneyLeft == 0) 1
      else if(moneyLeft < 0 || c.isEmpty) 0
      else count(moneyLeft - c.head, c) + count(moneyLeft, c.tail)
    }
    count(money, coins)
  }
}
