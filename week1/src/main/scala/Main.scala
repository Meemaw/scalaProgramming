import scala.annotation.tailrec

class Main {
  // ------------ Task 1. ---------------
  //  Write a function pyramid(n:int):String which returns pyramid with n lines
  //  For example pyramid(5) returns this:
  //      *
  //     ***
  //    *****
  //   *******
  //  *********

  def pyramid(n: Int): String = {

    def getString(number: Int, add: String): String = {

      @tailrec def loop(current: Int, returned_value: String): String = {
        if (current >= number) returned_value
        else loop(current + 1, returned_value + add)
      }

      loop(0,"")
    }


    @tailrec def mainLoop(current : Int, returned_value: String) : String = {
        if(current>= n) returned_value
        else {
          val whiteSpaces: String = getString(n - current - 1, " ")
          val stars: String = getString(1 + 2 * current, "*")
          val newline = whiteSpaces + stars + "\n"
          mainLoop(current + 1, returned_value + newline)
        }
    }
    mainLoop(0,"")

  }

  // ------------ Task 2. ---------------
  //  Write a function which takes string and returns same string without duplicated in sequence.
  //  For example for input aaabbbcca return abca
  def uniq(s: String): String = {

    @tailrec def loop(string_left : String, returned_value : String): String = {
      if(string_left.isEmpty()) returned_value
      else if(!string_left.tail.isEmpty() && string_left.head == string_left.tail.head) loop(string_left.tail, returned_value)
      else loop(string_left.tail, returned_value + string_left.head)
    }

    loop(s,"")
  }

  // ------------ Task 3. ---------------
  // Write a function that returns whether an integer is (probably) Lycherel's number
  // https://projecteuler.net/problem=55



  def isLycherel(n: BigInt): Boolean = {

    @tailrec def checkNumber(toCheck : BigInt, current : Int, bound: Int): Boolean = {

      @tailrec def reverse(toReverse: BigInt, reversed: BigInt): BigInt = {
        if(toReverse == 0) reversed
        else reverse(toReverse / 10, reversed * 10 + toReverse % 10)
      }

      def isPalindrome(checkPalindrome: BigInt): Boolean = {
        checkPalindrome.toString.reverse == checkPalindrome.toString
      }

      val x = toCheck + reverse(toCheck,0)
      if(current >= bound) true
      else if(isPalindrome(x)) false
      else checkNumber(x, current + 1, bound)
    }

    checkNumber(n,0,50)
  }


}
