import org.specs2.mutable._
import scala.collection.immutable.HashMap

class Week5Tests extends Specification {



	"Expr object" should {


		"Create object" in {
			5 mustEqual 5
			val test = Expr("a 5 + 4 / 2 c ^5 + *")
			test.toString mustEqual "Prod(Quot(Sum(Var(a),Num(5.0)),Num(4.0)),Sum(Num(2.0),Pow(Var(c),5)))"

		}

		"Calculate value" in {
			val glavni = Expr("a 5 + 4 / 2 c ^5 + *")
			val test = Expr("5")
			val test2 = Var("a")
			val test3 = Expr("5 5 + 5 *")
			val test4 = Expr("5 5 / 9 + a *")
			val h = HashMap("a" -> 17.0)
			val c = HashMap("a" -> 131.0, "c" -> 2.0)
			test.value() mustEqual Some(5.0)
			test2.value() mustEqual None
			glavni.value() mustEqual None
			test2.value(h) mustEqual Some(17.0)
			test3.value() mustEqual Some(50.0)
			test4.value(h) mustEqual Some(170.0)
			test4.value(c) mustEqual Some(1310.0)
			glavni.value(c) mustEqual Some(1156.0)
		}

		"Derive function" in {
			val test = Expr("5")
			val a = Var("a")
			val b = Var("b")
			val test2 = Expr("a")
			val testSum = Expr("5 5 +")
			val testSum2 = Expr("a b +")
			val testProd = Expr("5 5 *")
			val testProd2 = Expr("a 5 * 5 +")
			val testPow = Expr("a ^5")
			val testPow2 = Expr("5 ^5")
			val testPow3 = Expr("5 5 ^5 *")
			val testQout = Expr("5 5 /")
			val testDeriv = Expr("1 2 + 3 +")
			test.derive(a) mustEqual Expr("0")
			test2.derive(a) mustEqual Expr("1")
			test2.derive(b) mustEqual Expr("a")
			testSum.derive(a) mustEqual Expr("0 0 +")
			testSum2.derive(a) mustEqual Expr("1 b +")
			testDeriv.derive(a) mustEqual Expr("0 0 + 0 +")

			val testingS = testProd.derive(a)

			testProd.derive(a) mustEqual Expr("0 5 * 5 0 * +")
			testProd2.derive(a) mustEqual Expr("1 5 * a 0 * + 0 +")
			testPow.derive(a) mustEqual Expr("5 a ^4 *")
			testPow2.derive(a) mustEqual Expr("0")
			testPow3.derive(a) mustEqual Expr("0 5 ^5 * 5 0 * +")
			testQout.derive(a) mustEqual Expr("0 5 * -1 5 0 * * + 5 ^2 /")
			testingS.simplify mustEqual Expr("0")
			testQout.derive(a).simplify mustEqual Expr("0")




			val testDerive = Expr("a ^2 6 * 5 +")
			testDerive.derive(a).simplify mustEqual Expr("2 a * 6 *") 

		}

		"Simplyfy expression" in {
			val test1 = Expr("0 5 +")
			val test2 = Expr("5 0 +")
			val test3 = Expr("0 0 +")
			val test4 = Expr("5 5 +")
			val testProd = Expr("5 5 *")
			val testProd2 = Expr("1 5 *")
			val testProd3 = Expr("5 0 *")
			val testQout = Expr("8 1 /")
			val testQout2 = Expr("0 100 /")
			val testPow = Expr("5 ^1")
			val testPow2 = Expr("5 ^0")
			test1.simplify mustEqual Num(5.0)
			test2.simplify mustEqual Num(5.0)
			test3.simplify mustEqual Num(0.0)
			test4.simplify mustEqual Expr("5 5 +")
			testProd.simplify mustEqual Expr("5 5 *")
			testProd2.simplify mustEqual Expr("5")
			testProd3.simplify mustEqual Expr("0")
			testQout.simplify mustEqual Expr("8")
			testQout2.simplify mustEqual Expr("0")
			testPow.simplify mustEqual Expr("5")
			testPow2.simplify mustEqual Expr("1")


		}

	}

}
