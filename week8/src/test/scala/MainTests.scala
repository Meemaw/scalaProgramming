import org.specs2.mutable._

class Week8Tests extends Specification {

	"Infinite lists" should {
		"from" in {
			InfLists.from(1).head mustEqual 1
			InfLists.from(1).take(10).toList mustEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
			InfLists.from(3).take(10).toList mustEqual List(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
			InfLists.from(343).take(10).toList mustEqual List(343, 344, 345, 346, 347, 348, 349, 350, 351, 352)
		}

		"get e^x, sin and cos coeficients" in {
			InfLists.exCoef.take(10).toList mustEqual List(1.0, 1.0, 0.5, 0.16666666666666666, 0.041666666666666664, 0.008333333333333333, 0.001388888888888889, 1.9841269841269839E-4, 2.4801587301587298E-5, 2.755731922398589E-6)
			InfLists.cosCoef.take(10).toList mustEqual List(1.0, -0.5, 0.041666666666666664, -0.001388888888888889, 2.4801587301587298E-5, -2.7557319223985894E-7, 2.08767569878681E-9, -1.1470745597729723E-11, 4.779477332387385E-14, -1.5619206968586228E-16)
			InfLists.sinCoef.take(10).toList mustEqual List(1.0, -0.16666666666666666, 0.008333333333333333, -1.9841269841269839E-4, 2.755731922398589E-6, -2.5052108385441717E-8, 1.6059043836821613E-10, -7.647163731819816E-13, 2.811457254345521E-15, -8.22063524662433E-18)
		}
	}

  //test Recaman - funckija ni implementirana
  "Collatz theroy" should {
  	// pomožna funkcija
  	"get naturals stream" in {
  		Collatz.naturals.take(10).toList mustEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  	}
  	"get trace" in {
  		Collatz.trace(7).toList mustEqual List(7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1)
  		Collatz.trace(256).toList mustEqual List(256, 128, 64, 32, 16, 8, 4, 2, 1)
  		Collatz.trace(19).toList mustEqual List(19, 58, 29, 88, 44, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1)
  		Collatz.trace(1).toList mustEqual List(1)
  		Collatz.trace(3).toList mustEqual List(3, 10, 5, 16, 8, 4, 2, 1)
  	}
  	"get all traces" in {
  		val all = Collatz.allTraces
  		all.head.toList mustEqual List(1)
  		all.tail.head.toList mustEqual List(2,1)
  		all.tail.tail.head mustEqual Collatz.trace(3)
  		all.tail.tail.tail.tail.head mustEqual Collatz.trace(5)
  	}
  	"find max steps" in {
  		Collatz.findMaxSteps(10) mustEqual (9,20)
  		Collatz.findMaxSteps(100) mustEqual (97,119)
  		Collatz.findMaxSteps(1000) mustEqual (871,179)
  		Collatz.findMaxSteps(5000) mustEqual (3711,238)
  		Collatz.findMaxSteps(8500) mustEqual (6171,262)
  		Collatz.findMaxSteps2(8500) mustEqual (6171,262)
  		Collatz.findMaxSteps3(8500) mustEqual (6171,262)
  		Collatz.findMaxSteps4(8500) mustEqual (6171,262)
  	}

  }

  "PowerSeries" should {
  	"Get e^x, sin and cos power series" in {
  		PowerSeries.ex.coef.take(10).toList mustEqual List(1.0, 1.0, 0.5, 0.16666666666666666, 0.041666666666666664, 0.008333333333333333, 0.001388888888888889, 1.9841269841269839E-4, 2.4801587301587298E-5, 2.755731922398589E-6)
  		PowerSeries.cos.coef.take(10).toList mustEqual List(1.0, 0.0, -0.5, 0.0, 0.041666666666666664, 0.0, -0.001388888888888889, 0.0, 2.4801587301587298E-5, 0.0)
  		PowerSeries.sin.coef.take(10).toList mustEqual List(0.0, 1.0, 0.0, -0.16666666666666666, 0.0, 0.008333333333333333, 0.0, -1.9841269841269839E-4, 0.0, 2.755731922398589E-6)
  	}

	val pex = PowerSeries.ex
	val psin = PowerSeries.sin
	val pcos = PowerSeries.cos
  	"Get a stream of better values" in {
  		pex(1).take(10).toList mustEqual List(1.0, 2.0, 2.5, 2.666666666666667, 2.7083333333333335, 2.716666666666667, 2.718055555555556, 2.7182539682539684, 2.71827876984127, 2.7182815255731922)
  		pex(1)(50) must beCloseTo (2.718, 0.001) 
  		pex(2)(50) must beCloseTo (7.389, 0.001) 
		pex(10)(50) must beCloseTo (22026.46579, 0.001) 

		psin(3.14)(20) must beCloseTo (0, 0.1)
		psin(1.57)(20) must beCloseTo (1, 0.1)
		psin(0)(20) must beCloseTo (0, 0.1)

		pcos(3.14)(20) must beCloseTo (-1, 0.1)
		pcos(1.57)(20) must beCloseTo (0, 0.1)
		pcos(0)(20) must beCloseTo (1, 0.1)
  	}
  	"add two powerseries" in {
  		(pex + pex)(1)(50) must beCloseTo (5.4365, 0.001) 
  	}
  	"derive powerseries" in {
  		pex.derive(1)(50) must beCloseTo (2.718, 0.001) 
  		psin.derive(0)(50) must beCloseTo (pcos(0)(50), 0.001)
  		pcos.derive(0)(50) must beCloseTo (psin(0)(50), 0.001)
  		PowerSeries.sin.derive(1).take(10).toList mustEqual PowerSeries.cos(1).take(10).toList
  	}

  }

}

