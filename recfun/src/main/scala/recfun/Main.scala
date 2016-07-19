package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    val counts = countChange(4, List(1, 2))
    println(counts)
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if ((c == 0) || (c == r)) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      @tailrec
      def loop(acc: Int, chars: List[Char]): Int = {
        if (chars.isEmpty) acc
        else if (acc < 0) acc
        else if (chars.head == '(') loop(acc + 1, chars.tail)
        else if (chars.head == ')') loop(acc - 1, chars.tail)
        else loop(acc, chars.tail)
      }
      loop(0, chars) == 0
  }
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else countChange(money, coins.tail) + countChange(money-coins.head, coins)
  }
  }
