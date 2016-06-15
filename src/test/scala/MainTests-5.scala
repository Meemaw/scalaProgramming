import org.specs2.mutable._
import scala.collection.immutable.HashMap

class Week5Tests extends Specification {
  //testi za ustvarjanje izrazov
  "testi za ustvarjanje izrazov" should {
    	"pravilno ustvarjanje" in {
    		var a = Expr("x ^4 2 x ^3 * + 5 x ^2 * + x + 7 +")
    		var resA = Sum(Sum(Sum(Sum(Pow(Var("x"),4),Prod(Num(2.0),Pow(Var("x"),3))),Prod(Num(5.0),Pow(Var("x"),2))),Var("x")),Num(7.0))

    		var b = Expr("a 5 + 4 / 2 c ^5 + *")
    		var resB = Prod(Quot(Sum(Var("a"),Num(5.0)),Num(4.0)),Sum(Num(2.0),Pow(Var("c"),5)))
    		
    		var c = Expr("0")
    		var resC = Num(0.0)

    		var d = Expr("1 2 + 3 * 6 + 2 3 + /")
    		var resD = Quot(Sum(Prod(Sum(Num(1.0),Num(2.0)),Num(3.0)),Num(6.0)),Sum(Num(2.0),Num(3.0)))

    		a mustEqual resA
    		b mustEqual resB
    		c mustEqual resC
    		d mustEqual resD
    	}
    }
  
  //testi za ovrednotenje izrazov
  "testi za ovrednotenje izrazov" should {
    	"pravilna vrednost izraza brez neznank" in {
    		var a = Quot(Sum(Prod(Sum(Num(1.0),Num(2.0)),Num(3.0)),Num(6.0)),Sum(Num(2.0),Num(3.0)))
    		
    		var b = Pow(Sum(Prod(Num(-1.0),Num(2.0)),Num(-3.0)), 2)

    		a.value().get mustEqual 3.0
    		b.value().get mustEqual 25.0
    	}
    	"pravilna vrednost izraza z neznankami in podano HashMap" in {
    		var h1 = HashMap("x"->3.0)
    		var h2 = HashMap("a"->3.0, "c"->2.0)
    		var h3 = HashMap("x"->3.0, "c"->2.0)


    		var a = Sum(Sum(Sum(Sum(Pow(Var("x"),4),Prod(Num(2.0),Pow(Var("x"),3))),Prod(Num(5.0),Pow(Var("x"),2))),Var("x")),Num(7.0))
    		
    		var b = Prod(Quot(Sum(Var("a"),Num(5.0)),Num(4.0)),Sum(Num(2.0),Pow(Var("c"),5)))

    		a.value(h1) mustEqual Some(190.0)
    		a.value(h3) mustEqual Some(190.0)
    		b.value(h2) mustEqual Some(68.0)
    		b.value(h1) mustEqual None //neuporabna HashMap

    	}
    }

  //testi za odvajanje
  "testi za odvajanje" should {
    	"pravilno odvajanje" in {
    		var a = Sum(Prod(Num(2.0),Pow(Var("x"),2)), Num(4.0))

    		a.derive(Var("x")) mustEqual Sum(Sum(Prod(Num(2.0),Prod(Num(2.0),Pow(Var("x"),1))),Prod(Num(0.0),Pow(Var("x"),2))),Num(0.0))

    		var b = Sum(Sum(Sum(Sum(Pow(Var("x"),4),Prod(Num(2.0),Pow(Var("x"),3))),Prod(Num(5.0),Pow(Var("x"),2))),Var("x")),Num(7.0))

    		b.derive(Var("x")) mustEqual Sum(Sum(Sum(Sum(Prod(Num(4.0),Pow(Var("x"),3)),Sum(Prod(Num(2.0),Prod(Num(3.0),Pow(Var("x"),2))),Prod(Num(0.0),Pow(Var("x"),3)))),Sum(Prod(Num(5.0),Prod(Num(2.0),Pow(Var("x"),1))),Prod(Num(0.0),Pow(Var("x"),2)))),Num(1.0)),Num(0.0))
    		
    		var c = Quot(Var("x"), Prod(Num(2.0), Pow(Var("x"), 2)))
    		c.derive(Var("x")) mustEqual Quot(Sum(Prod(Num(1.0),Prod(Num(2.0),Pow(Var("x"),2))),Prod(Num(-1.0),Prod(Var("x"),Sum(Prod(Num(2.0),Prod(Num(2.0),Pow(Var("x"),1))),Prod(Num(0.0),Pow(Var("x"),2)))))),Pow(Prod(Num(2.0),Pow(Var("x"),2)),2))
    	}
    }

  //testi za poenostavljanje
  "testi za poenostavljanje ~ simplify" should {
    	"produkt z 0" in {	
    		var b = Prod(Num(0.0), Var("X"))
    		b.simplify mustEqual Num(0.0)	
    	}
    	"produkt z 1" in {	
    		var b = Prod(Num(1.0), Var("X"))
    		b.simplify mustEqual Var("X")	
    	}
    	"vsota z 0" in {	
    		var b = Sum(Num(0.0), Var("X"))
    		b.simplify mustEqual Var("X")	
    	}
    	"poracun vsote stevil" in {	
    		var b = Sum(Num(2.0), Num(6.0))
    		b.simplify mustEqual Num(8.0)	
    	}
    	"poracun produkta stevil" in {	
    		var b = Prod(Num(2.0), Num(6.0))
    		b.simplify mustEqual Num(12.0)	
    	}
    	"asociativnost produkta" in {	
    		var b1 = Prod(Num(2.0), Prod(Num(6.0),Var("x")))
    		var b2 = Prod(Num(2.0), Prod(Var("x"),Num(6.0)))
    		b1.simplify mustEqual Prod(Num(12.0), Var("x"))
			b2.simplify mustEqual Prod(Num(12.0), Var("x"))


			var b3 = Prod(Prod(Num(6.0),Var("x")), Num(2.0))
    		var b4 = Prod(Prod(Var("x"),Num(6.0)), Num(2.0))
    		b1.simplify mustEqual Prod(Num(12.0), Var("x"))
			b2.simplify mustEqual Prod(Num(12.0), Var("x"))
    	}
    	"deljenje nicle" in {	
    		var b = Quot(Num(0.0), Var("X"))
    		b.simplify mustEqual Num(0.0)	
    	}
    	"deljenje z 1" in {	
    		var b = Quot(Var("X"), Num(1.0))
    		b.simplify mustEqual Var("X")
    	}
    	"deljenje stevil" in {	
    		var b = Quot(Num(12.0), Num(6.0))
    		b.simplify mustEqual Num(2.0)
    	}
    	"potenca z osnovo 1" in {	
    		var b = Pow(Num(1.0), 134)
    		b.simplify mustEqual Num(1.0)
    	}
    	"potenca z eksponentom 1" in {	
    		var b = Pow(Var("X"), 1)
    		b.simplify mustEqual Var("X")
    	}
    	"izracun potence" in {	
    		var b = Pow(Num(2.0), 5)
    		b.simplify mustEqual Num(32.0)
    	}
    	"pravilno poenostavljanje ~kompleksnih~ primerov" in {
    		var a = Sum(Sum(Prod(Num(2.0),Prod(Num(2.0),Pow(Var("x"),1))),Prod(Num(0.0),Pow(Var("x"),2))),Num(0.0))
    		
    		a.simplify mustEqual Prod(Num(4.0),Var("x"))

    	}
    }

}
