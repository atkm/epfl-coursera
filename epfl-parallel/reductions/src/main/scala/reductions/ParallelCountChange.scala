package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && coins.nonEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0 // no coins left or money < 0
  }

  //  Implementation copied from the `recfun` project (1st assignment of 1st course).
  def countChangeLoop(money: Int, coins: List[Int]): Int = {
    if (money == 0 || coins.isEmpty) 0
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

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else if (threshold(money, coins)) countChange(money, coins)
    else {
      val (left, right) = parallel[Int, Int](
        parCountChange(money - coins.head, coins, threshold),
        parCountChange(money, coins.tail, threshold)
      )
      left + right
    }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    (m, cs) => m <= (startingMoney * 2)/3

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (m, cs) => cs.length <= (totalCoins * 2)/3


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (m, cs) => m * cs.length <= (startingMoney * allCoins.length)/2
  }
}
