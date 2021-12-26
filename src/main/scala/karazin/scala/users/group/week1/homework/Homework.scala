package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    def not(b: Boolean): Boolean =
      if (b == true) false
      else true

    def and(left: Boolean, right: Boolean): Boolean =
      if (left == false) false
      else if (right == false) false
      else true

    def or(left: Boolean, right: Boolean): Boolean =
      if(left == true) true
      else if(right == true) true
      else false

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (num1, num2) =>
      def multiply(num1: BigInt, num2: BigInt, acc: BigInt) : BigInt =
        if (num1 == 0 || num2 == 0)
          acc
        else if (num2 < 0)
          multiply(num1, num2 + 1, acc - num1)
        else
          multiply(num1, num2 - 1, acc + num1)
      multiply(num1, num2, 0)

    val power: (BigInt, BigInt) => BigInt = (num1, num2) =>
      def calculatePower(num1: BigInt, num2: BigInt, acc: BigInt) : BigInt =
        if (num2 > 0)
          calculatePower(num1, num2 - 1, acc * num1)
        else if (num2 < 0)
          calculatePower(num1, -num2, 1 / acc)
        else
          acc
      calculatePower(num1, num2, 1)

    val fermatNumber: Int => BigInt = (num) =>
      if(num < 0)
        0 
      else 
        power(2, power(2, num)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence` :

    def toLookAndSay(n: BigInt): BigInt = {
      val nList = n.toString.map(_.asDigit).toList

      @tailrec
      def inner(ints: List[Int], cur: BigInt, cnt: Int, acc: BigInt): BigInt = ints match {
        case List() => (acc * 10 + cnt) * 10 + cur
        case h :: t => {
          if (h == cur) inner(t, cur, cnt + 1, acc)
          else inner(t, h, 1, (acc * 10 + cnt) * 10 + cur)
        }
      }

      inner(nList.tail, nList.head, 1, 0)
    }

    val lookAndSaySequenceElement: Int => BigInt = n => {
      @tailrec
      def loop(cur: BigInt, left: BigInt): BigInt = left match {
        case 0 => cur
        case _ => loop(toLookAndSay(cur), left - 1)
      }

      loop(1, n)
    }

  end `Look-and-say Sequence`

end Homework