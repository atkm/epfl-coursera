package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   * Implementation copied from the `recfun` project (1st assignment of 1st course).
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
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

    checker(chars.toList, 0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    // the sequential part; use a while loop or make it tail-recursive
    @tailrec
    def traverse(idx: Int, until: Int, unmatchedLeft: Int, unmatchedRight: Int) : (Int, Int) = {
      //println(s"traverse called with ($unmatchedLeft, $unmatchedRight)")
      if (idx >= until) (unmatchedLeft, unmatchedRight)
      else chars(idx) match {
        case '(' => traverse(idx+1, until, unmatchedLeft+1, unmatchedRight)
        case ')' => {
          if (unmatchedLeft > 0) traverse(idx+1, until, unmatchedLeft-1, unmatchedRight)
          else traverse(idx+1, until, unmatchedLeft, unmatchedRight+1)
        }
        case _   => traverse(idx+1, until, unmatchedLeft, unmatchedRight)
      }
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      // sequential threshold
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        //println(s"from: $from; until = $until")
        val mid = from + (until - from)/2
        val ((leftOfLeft, rightOfLeft), (leftOfRight, rightOfRight))
        = parallel( reduce(from, mid), reduce(mid, until) )
        //println(s"lOL = $leftOfLeft; rOL = $rightOfLeft; lOR = $leftOfRight; rOR = $rightOfRight")
        // the number of unmatched parens that aren't killed by combining the left and right results.
        val diff = leftOfLeft - rightOfRight
        if (diff > 0) {
          val result = (leftOfRight + diff, rightOfLeft)
          //println(s"reduce returns $result")
          result
        }
        else {
          val result = (leftOfRight, rightOfLeft-diff)
          //println(s"reduce returns $result")
          result
        }
      }
    }

    val balance = reduce(0, chars.length)
    println(balance)
    balance == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
