package reductions

import scala.annotation._
import org.scalameter._

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
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def balanceIter(counter: Int, chars: Array[Char]): Boolean = {
      if (chars.isEmpty || counter < 0) counter == 0
      else if (chars.head == '(') balanceIter(counter + 1, chars.tail)
      else if (chars.head == ')') balanceIter(counter - 1, chars.tail)
      else balanceIter(counter, chars.tail)
    }

    balanceIter(0, chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
      var index = idx
      var (leftAcc: Int, rightAcc: Int) = (arg1, arg2)

      while (index < until || index == idx) {
        if (chars(index) == '(')
          leftAcc += 1

        else if (chars(index) == ')') {
          if (leftAcc == 0)
            rightAcc -= 1
          else
            leftAcc -= 1
        }

        index += 1
      }

      (leftAcc, rightAcc)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold)
        traverse(from, until, 0, 0)

      else {
        val (v1, v2) = parallel(reduce(from, from + (until - from) / 2),
                                reduce(from + (until - from) / 2, until))

        if (v1._2 == 0 && v2._1 == 0) {
          if (v1._1 > -v2._2)
            (v1._1 + v2._2, 0)
          else
            (0, v2._2 + v1._1)
        }

        else
          (v1._1 + v2._1, v1._2 + v2._2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
