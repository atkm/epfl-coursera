package recfun

import scala.annotation.tailrec

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
    def factorial(n: Int): Int = {
      @tailrec
      def accumulator(acc: Int, n: Int): Int = {
        if (n == 0) acc
        else accumulator(n*acc, n-1)
      }
      accumulator(1, n)
    }

    def nChooseK(n: Int, k: Int): Int = {
      if (k > n) throw new IllegalArgumentException("k greater than n")
      else factorial(n)/(factorial(k) * factorial(n-k))
    }

    nChooseK(r, c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def checker(chars: List[Char], left: Int, right: Int): Boolean = {
      if (right > left) false
      else
        chars match {
          case Nil => left == right
          case '('::tail => checker(tail, left+1, right)
            case ')'::tail => checker(tail, left, right+1)
          case _::tail => checker(tail, left, right)
        }
    }

    checker(chars, 0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 || coins.size == 0) 0
    else {
      val table = Array.ofDim[Int](money+1, coins.size)
      List.range(0, coins.size).foreach( table(0)(_) = 1 )

      for (j <- List.range(0, coins.size); m <- List.range(1, money+1)) {
        val x = if (m - coins(j) >= 0) table(m - coins(j))(j) else 0
        val y = if (j >= 1) table(m)(j-1) else 0
        table(m)(j) = x+y
      }

      table(money)(coins.size-1)
    }
  }
}
