import org.specs2.mutable._

class Week7Tests extends Specification {

  // AlgebraStruct tests

  val s = List(0,1,2)
  val op1 = (x:Int,y:Int) => (x+y) % 3
  val op2 = (x:Int, y:Int) => (x+y)
  val op3 = (x:Int, y:Int) => (x-y)
  val op4 = (x:Int, y:Int) => (x*y)
  val op5 = (x:Int, y:Int) => 5
  val op10 = (x:Int, y:Int) => (x+y) % 5
  val op11 = (x:Int, y:Int) => (x+y) % 17
  val op12 =(x:Int, y:Int) => (x-y) % 5


  "Algebra struct" should {
  

  	"isClosed" in {
  		AlgebraStruct(s,op1).isClosed mustEqual true
  		AlgebraStruct(s,op2).isClosed mustEqual false
      AlgebraStruct(s,op10).isClosed mustEqual false

  	}

  	"isCommut" in {
  		AlgebraStruct(s,op1).isCommut mustEqual true
  		AlgebraStruct(s,op3).isCommut mustEqual false
  	}

  	"isAssoc" in {
  		AlgebraStruct(s,op1).isAssoc mustEqual true
  		AlgebraStruct(s,op3).isAssoc mustEqual false
  	}

  	"hasUnit" in {
  		AlgebraStruct(s, op1).hasUnit mustEqual true
  		AlgebraStruct(s, op5).hasUnit mustEqual false

     // AlgebraStruct(s,op2).unit.get mustEqual 0 // seštevanje
      //AlgebraStruct(s,op3).unit.get mustEqual 0 // odštevanje
      //AlgebraStruct(s,op4).unit.get mustEqual 1 // mnozenje
      //AlgebraStruct(s,op1).unit.get mustEqual 0
  	}

    
    "hasInverse" in {
      AlgebraStruct(s, op4).hasInverse(1) mustEqual true
      AlgebraStruct(s, op1).hasInverse(1) mustEqual true
    }
    


  }

  "closureTests" should {


    "Close operation" in{
      AlgebraStruct(s,op12).isClosed mustEqual false
      AlgebraStruct(s,op10).isClosed mustEqual false
      AlgebraStruct(s,op11).isClosed mustEqual false


      AlgebraStruct(s,op10).makeGrupoid.isClosed mustEqual true
      AlgebraStruct(s,op11).makeGrupoid.isClosed mustEqual true
      AlgebraStruct(s,op12).makeGrupoid.isClosed mustEqual true
    }
  }



  "RList tests" should {


      val x = RList(5)
      val y = x.add(3)
      val z = y.add(6)
      val c = z.add(9)
      val vlki = RList(0,1,2,3,4,5,6,7)
      val vecji = vlki.add(10)

    "Create RList" in {
      x.length mustEqual 1
      y.length mustEqual 2
      z.length mustEqual 3
     // c.length mustEqual 4
     x.toString mustEqual List(5).toString
     y.toString mustEqual List(3, 5).toString
     z.toString mustEqual List(6,3,5).toString
     vecji.length mustEqual 9

     vlki.toString mustEqual List(0,1,2,3,4,5,6,7).toString
     vecji.toString mustEqual List(10,0,1,2,3,4,5,6,7).toString

    }

    "Check head" in {
      x.head mustEqual 5
      y.head mustEqual 3
      z.head mustEqual 6
      vecji.head mustEqual 10
    }

    "get nth element" in {
      vlki(0) mustEqual 0
      vlki(1) mustEqual 1
      vlki(2) mustEqual 2
      vecji(0) mustEqual 10
      vlki(5) mustEqual 5
    }

    "get tail" in {
      x.tail.toString mustEqual Nil.toString
      y.tail.toString mustEqual List(5).toString
      c.tail.toString mustEqual List(6,3,5).toString
      vecji.tail.toString mustEqual List(0,1,2,3,4,5,6,7).toString
    }

    "update rlist" in {
      vecji.update(0, 5).toString mustEqual List(5,0,1,2,3,4,5,6,7).toString
      vecji.update(3, 100).toString mustEqual List(10,0,1,100,3,4,5,6,7).toString
      vecji.update(1,2).toString mustEqual List(10,2,1,2,3,4,5,6,7).toString
      vecji.update(0,5).update(1,5).update(2,5).update(3,5).update(8,5).toString mustEqual List(5,5,5,5,3,4,5,6,5).toString
    }


  }
  //Closure tests



  //RList tests

}
