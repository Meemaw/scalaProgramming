import org.specs2.mutable._

class Week1Tests extends Specification {
  val solution = new Main

  /* Testing the pyramid function */
  "The pyramid function" should {
    "do nothing for non positive n" in {
      solution.pyramid(-1) must have size (0)
    }
    "draw a nice pyramid" in {
      solution.pyramid(5) mustEqual ("""    *
   ***
  *****
 *******
*********
""")

      solution.pyramid(1) mustEqual ("""*
""")
      solution.pyramid(2) mustEqual(
""" *
***
""")
    }

    "size of pyramids" in {
      solution.pyramid(1) must have size(2)
      solution.pyramid(2) must have size(7)
      solution.pyramid(3) must have size(15)
      solution.pyramid(4) must have size(26)
    }
    //Add a few more tests
  }
  /* testing the uniq function */
  "The uniq function" should {
    "do nothing if there are no dups." in {
      solution.uniq("ababa") mustEqual ("ababa")
      solution.uniq("abcbca") mustEqual ("abcbca")
    }

    "remove only consecutive dup." in {
      solution.uniq("aabbb") mustEqual ("ab")
      solution.uniq("aaaaaaaaaa") mustEqual ("a")
    }

    "check for empty string." in {
      solution.uniq("") mustEqual ("")
    }

    "whitespace" in {
      solution.uniq("        ") mustEqual(" ")
    }

    "random text" in {
      solution.uniq("danes je leeeeep  daann. Jutri bo soonččno.") mustEqual("danes je lep dan. Jutri bo sončno.")
    }
  }

  /* testing isLycherel function */
  "The isLycherel function" should {
    "return false if is not Lycherel " in {
      solution.isLycherel(956) mustEqual (false)
      solution.isLycherel(0) mustEqual (false)
      solution.isLycherel(294) mustEqual (false)
      solution.isLycherel(1604) mustEqual(false)
      solution.isLycherel(2069) mustEqual(false)

    }

    "return true if it is Lycherel" in {
      solution.isLycherel(196) mustEqual (true)
      solution.isLycherel(9597) mustEqual (true)
      solution.isLycherel(9988) mustEqual (true)


    }
  }

  "palindrome at start" in {
    solution.isLycherel(9999) mustEqual (true)
    solution.isLycherel(4994) mustEqual (true)


  }

}
