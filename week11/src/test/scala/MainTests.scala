import org.specs2.mutable._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck._

// primer testov s Scalacheck
object ExampleSpecification extends Properties("Example") {

  property("startsWith") = forAll { (a: String, b: String) =>
    (a + b).startsWith(a)
  }

  property("concatenate") = forAll { (a: String, b: String) =>
    (a + b).length >= a.length && (a + b).length >= b.length
  }

}

// testi za nalogo s Scalacheck
object Naloga1Specification extends Properties("Naloga scalacheck") {


	property("encode") = forAll { (s: String, i: Int) =>
		s.length == ScalacheckEx.encode(s,i).length
	}

	property("decode") = forAll { (s:String, move:Int) =>
		s.length == ScalacheckEx.decode(s,move).length
		ScalacheckEx.decode(ScalacheckEx.encode(s,move), move) == s
	}

	property("duplicateList") = forAll { (l:List[Int]) =>
		l.length*2 == ScalacheckEx.duplicate(l).length
	}

	property("sort") = forAll { (l:List[Int]) =>

		l.sortWith(_>_) == ScalacheckEx.sort(l)
		l.length == ScalacheckEx.sort(l).length
	}


}

// testi za ponovitveno nalogo
class Week11Tests extends Specification {


	"week11Tests" should {

		"create NAT" in {
			Nat(5).evaluate mustEqual 5
			Nat(0).evaluate mustEqual 0
			Nat(1).evaluate mustEqual 1

		}

		"operations on Nat" in {
			(Nat(10)+Nat(5)).evaluate mustEqual 15
			(Nat(10)*Nat(2)).evaluate mustEqual 20
			(Nat(10)*Nat(0)).evaluate mustEqual 0
			(Nat(10)-Nat(10)).evaluate mustEqual 0
			(Nat(100)-Nat(25)).evaluate mustEqual 75
			(Nat(5)<Nat(10)) mustEqual true
		}


		"complexNumbers" in {
			val t1 = C(1,2)
			val t2 = C(2,2)
			val t3 = C(0,0)
			(t1+t2).evaluate mustEqual 5
			(t2-t1).evaluate mustEqual 1
			val stream = Mandelbrot.cSeq(t1)
			println(Mandelbrot.draw)
			5 mustEqual 5
		}

	}



}
