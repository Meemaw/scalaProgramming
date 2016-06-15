import org.specs2.mutable._

class Week2Tests extends Specification {
  val solution = new Main
  val eps = 0.00001

  "The set functions" should {
    "create the correct singleton" in {
      val s1 = solution.singleton(5)
      val s2 = solution.singleton(10)
      s1(5) mustEqual true
      s1(4) mustEqual false
      s1(6) mustEqual false
      s2(4) mustEqual false
      s2(10) mustEqual true
    }
    "create the correct union" in {
      val s1 = solution.singleton(5)
      val s2 = solution.singleton(10)
      val s3 = solution.union(s1,s2)
      s3(4) mustEqual false
      s3(5) mustEqual true
      s3(10) mustEqual true

    }
    "create the correct intersect" in {
      val s1 = solution.singleton(5)
      val s2 = solution.singleton(10)
      val s4 = solution.singleton(5)
      val s3 = solution.intersect(s1,s2)
      val s5 = solution.intersect(s1,s4)
      s3(5) mustEqual false
      s3(10) mustEqual false
      s5(5) mustEqual true
      s5(10) mustEqual false

    }

    "build interval from a to b" in {
      val t1 = solution.interval(5,10)
      t1(5) mustEqual true
      t1(8) mustEqual true
      t1(10) mustEqual true
      t1(12) mustEqual false
      t1(4) mustEqual false
      t1(11) mustEqual false




    }

    "remove all duplicated from list" in {
      val l1 = List(1,5,7,10,5,7)
      val l2 = List(1,1,1,1,1,1,1)
      val l3 = List()
      val t1 = solution.dedup(l1)
      val t2 = solution.dedup(l2)
      val t3 = solution.dedup(l3)
      t1 mustEqual List(1,5,7,10)
      t2 mustEqual List(1)
      t3 mustEqual List()

    }



    //etc. TODO

  }

  "The real functions" should {
    "sum of two functions" in {
      val test = solution.sum(x => x*5,x => x*5)
      val test2 = solution.sum(x => x*5,x => x)
      val test3 = solution.sum(x => x*5,x => -x)
      val test4 = solution.sum(x => x,x => -x)
      test4(10) mustEqual 0
      test(0) mustEqual 0
      test3(-5) mustEqual -20
      test3(10) mustEqual 40
      test(10) mustEqual 100
      test2(10) mustEqual 60
    }

    "compositum of two functions" in {
      val test = solution.compose(x => x*5, x => x)
      val powerTest = solution.compose(x => x*5,x => x*5)
      val test1 = solution.compose(x => x*5,x => -x)
      val test2 = solution.compose(x => x,x => -x)
      test(10) mustEqual 50
      test1(10) mustEqual -50
      test2(10) mustEqual -10
      powerTest(10) mustEqual 250
      test(0) mustEqual 0
    }

    "multiply two functions" in {
      val test = solution.mult(x => x*5,x => x)
      val test2 = solution.mult(x => x*5,x => -x)
      val test3 = solution.mult(x => x,x => -x)
      val powerTest = solution.mult(x => x*5,x => x*5)
      test(10) mustEqual 500
      test2(10) mustEqual -500
      test3(10) mustEqual -100
      powerTest(10) mustEqual 50*50
    }

    "derivative of function" in {
      def func: Double => Double = x => x +1
      def func1: Double => Double = x => 2*x + 1
      def func2: Double => Double = x => x*x + 1
      def func3: Double => Double = x => x*x + x
      def derivate = solution.derive(func)
      def derivate1 = solution.derive(func1)
      def derivate2 = solution.derive(func2)
      def derivate3 = solution.derive(func3)
      derivate(5) should (be > 1-2*eps and be < 1+2*eps)
      derivate(0) should (be > 1-2*eps and be < 1+2*eps)
      derivate1(5) should (be > 2-2*eps and be < 2+2*eps)
      derivate2(5) should (be > 10-2*eps and be < 10+2*eps)
      derivate3(5) should (be > 11-2*eps and be < 11+2*eps)
    }
    //TODO
  }

  "The coding functions" should {
    "encode function" in {
      def test(encoder : Char => Char): String = solution.encode("abc", encoder)
      test(x => 'a') mustEqual "aaa"
      test(x => ((x.toInt)+1).toChar) mustEqual "bcd"
      test(x => ((x.toInt)+3).toChar) mustEqual "def"
      test(x => '1') must have size(3)
      test(x => x) mustEqual "abc"
      solution.encode("", x => '1') mustEqual ""
    }

    "is bijective encoder" in {
      def test(f: Char => Char) = solution.isBijective(f)
      test(x => x) mustEqual true
      test(x => ((x.toInt)+1).toChar) mustEqual true
      test(x => 'a') mustEqual false
      test(x => if(x.toInt%2 == 0) '1' else x) mustEqual false
      test(x => ((x.toInt)-1).toChar) mustEqual true


    }

    "get inverse of coder" in {
      val string = "abc"
      def firstCoder: Char => Char = x => x
      def secondCoder: Char => Char = x => ((x.toInt)+1).toChar
      def thirdCoder: Char => Char = x => ((x.toInt)-1).toChar

      val zakodiraj1 = solution.encode(string, firstCoder)
      val zakodiraj2 = solution.encode(string, secondCoder)
      val zakodiraj3 = solution.encode(string, thirdCoder)

      val odkodiraj1 = solution.encode(zakodiraj1, solution.inverse(firstCoder).get)
      val odkodiraj2 = solution.encode(zakodiraj2, solution.inverse(secondCoder).get)
      val odkodiraj3 = solution.encode(zakodiraj3, solution.inverse(thirdCoder).get)



      odkodiraj1 mustEqual "abc"
      odkodiraj2 mustEqual "abc"
      odkodiraj3 mustEqual "abc"
      solution.inverse(x => '1') mustEqual None

    }

    "decode string that was encoded with coder" in {
      val encoded = "bcdef"
      val decoded1 = solution.decode(encoded, x => ((x.toInt)+1).toChar)
      val decoded2 = solution.decode(encoded, x => x)
      val decoded3 = solution.decode(encoded, x => ((x.toInt)-1).toChar)
      decoded1 mustEqual "abcde"
      decoded2 mustEqual "bcdef"
      decoded3 mustEqual "cdefg"



    }
    //TODO
  }
}
