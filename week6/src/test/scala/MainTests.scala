import org.specs2.mutable._
import scala.io.Source

class Week6Tests extends Specification {

   //testi za Q1


   "naive queue implementation" should {


   	"work on 1 element" in {
   		val q = QEmpty1
   		val t1 = q.enQ(List(10))
   		t1.deQ._2 mustEqual List(10)

   		val t2 = q.enQ(List(5,5))
   		t2.deQ._2 mustEqual List(5,5)
   	}


   	"work on multiple elements" in {
   		val k = QEmpty1
   		val q = k.enQ(List(10)).enQ(List(15)).enQ(List(200,50))
   		q.deQ._2 mustEqual List(10)
   		q.deQ._1.deQ._2 mustEqual List(15)
   		q.deQ._1.deQ._1.deQ._2 mustEqual List(200,50)



   	}

   	"count number of steps" in {
   		val seq = List(Some(List(1)), Some(List(2)), None, None )
         val seq2 = List(Some(List(1)), Some(List(2)), None, None, Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)))
         val seq3 = List(Some(List(1)), Some(List(2)), None, None, Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None)   
         val seqUltimate = List(Some(List(1)), Some(List(2)), None, None, Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)),
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None,
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None)
          val seq5 = List(Some(List(9, 20, 17, 20, 6, 14, 10, 19, 18, 7, 4, 20)), None, Some(List(5, 12, 2, 7, 19, 20, 18, 11, 16, 13, 11, 16, 3, 6, 6, 4, 16, 20)), Some(List(15, 6, 15, 2, 9, 18, 17, 6, 20, 8, 14, 2, 9)), Some(List(10, 3, 16, 14, 18, 19, 1, 8, 19, 14, 14, 1, 4, 9, 11, 13, 11, 20, 10)), Some(List(8, 2, 4, 12, 16, 13, 16, 15, 20, 16)), Some(List(7, 17, 9, 17, 5, 4, 9, 11, 14, 12, 11, 15, 2, 8, 7, 11, 6, 4, 5)), None, None, None, Some(List(16, 18, 15, 18, 10, 20, 4, 10, 4, 12, 3)), Some(List(4, 5, 7, 4, 18, 13, 18, 14, 5, 8)), None, Some(List(12, 6, 3, 15, 12, 10, 13, 20, 6, 17, 15, 2, 8, 17, 12)), None, Some(List(3, 6, 18, 10, 15, 3, 11, 5, 15, 16, 9, 6, 8, 16, 14, 1, 4, 4, 5, 4)), Some(List(11, 5, 8, 11, 19, 16, 1, 14, 8, 18, 4, 11, 16, 8, 10, 17, 6, 2, 16, 13)), None, Some(List(9, 7, 1, 7, 9, 4, 14, 20, 11, 10, 14, 14, 6, 4)), Some(List(11, 5, 8, 11, 19, 16, 1, 14, 8, 18, 4, 11, 16, 8, 10, 17, 6, 2, 16, 13)), Some(List(5, 5, 4, 1, 15, 3, 15, 16, 2, 11, 6, 19, 5, 20, 2, 6)), Some(List(5, 5, 4, 1, 15, 3, 15, 16, 2, 11, 6, 19, 5, 20, 2, 6)), Some(List(1, 13, 7, 6, 17, 7, 20, 8, 12, 5, 5, 9, 6, 18, 14, 1, 9, 8, 18, 5)), Some(List(4, 1, 18, 8, 19, 2, 20, 12, 20, 9, 4, 5, 10, 1, 10, 16, 15, 6)), None, Some(List(12, 16, 11, 11, 5, 14, 11, 4, 8, 19, 11, 5, 3, 13, 15, 7)), None, Some(List(1, 14, 2, 5, 3, 2, 10, 18, 10, 9, 13, 5, 11, 10, 19, 19, 16, 18)), None, Some(List(18, 5, 1, 17, 12, 11, 8, 7, 2, 11, 17, 10, 10, 2, 6, 19, 9)), Some(List(11, 16, 2, 20, 15, 13, 3, 13, 8, 19)), Some(List(14, 7, 3, 16, 17, 11, 14, 14, 19, 4, 9, 14, 9, 13, 13, 15)), Some(List(15, 12, 15, 5, 14, 7, 14, 3, 2, 17, 8, 13, 17, 13, 19, 6, 14, 12, 16)), None, None, Some(List(16, 19, 9, 20, 15, 1, 13, 3, 19, 14, 1, 18)), Some(List(16, 12, 5, 8, 20, 9, 7, 7, 17, 4, 2, 18, 13, 2, 18, 19, 11)), Some(List(2, 6, 11, 18, 9, 3, 9, 15, 14, 11, 11, 20, 5, 16)), Some(List(4, 12, 12, 5, 4, 14, 14, 11, 18, 19)), None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
         val count = QEmpty1
         Q1.countAcc(seq5) mustEqual 201
         Q1.countAcc(seq) mustEqual 3
         Q1.countAcc(seq2) mustEqual 18
         Q1.countAcc(seq3) mustEqual 59
         Q1.countAcc(seqUltimate) mustEqual 6017
   	}

   }

   "Improved queue implementation" should {
      "work with 1 element" in {
         val q = QEmpty2
         val enq = q.enQ(List(10))
         enq.deQ._2 mustEqual(List(10))
      }

      "work with multiple elements" in {
         val c = QEmpty2.enQ(List(10)).enQ(List(20)).enQ(List(50))
         c.deQ._2 mustEqual List(10)
         c.deQ._1.deQ._2 mustEqual List(20)
         c.deQ._1.deQ._1.deQ._2 mustEqual List(50)

      }

      "count number of steps" in {
         val seq = List(Some(List(1)), Some(List(2)), None, None )
         val seq2 = List(Some(List(1)), Some(List(2)), None, None, Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)))
         val seq3 = List(Some(List(1)), Some(List(2)), None, None, Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None)
         val seqUltimate = List(Some(List(1)), Some(List(2)), None, None, Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)),
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None,
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None, 
          Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), Some(List(2)), None)
          val seq4 = List(Some(List(9, 20, 17, 20, 6, 14, 10, 19, 18, 7, 4, 20)), None, Some(List(5, 12, 2, 7, 19, 20, 18, 11, 16, 13, 11, 16, 3, 6, 6, 4, 16, 20)), Some(List(15, 6, 15, 2, 9, 18, 17, 6, 20, 8, 14, 2, 9)), Some(List(10, 3, 16, 14, 18, 19, 1, 8, 19, 14, 14, 1, 4, 9, 11, 13, 11, 20, 10)), Some(List(8, 2, 4, 12, 16, 13, 16, 15, 20, 16)), Some(List(7, 17, 9, 17, 5, 4, 9, 11, 14, 12, 11, 15, 2, 8, 7, 11, 6, 4, 5)), None, None, None, Some(List(16, 18, 15, 18, 10, 20, 4, 10, 4, 12, 3)), Some(List(4, 5, 7, 4, 18, 13, 18, 14, 5, 8)), None, Some(List(12, 6, 3, 15, 12, 10, 13, 20, 6, 17, 15, 2, 8, 17, 12)), None, Some(List(3, 6, 18, 10, 15, 3, 11, 5, 15, 16, 9, 6, 8, 16, 14, 1, 4, 4, 5, 4)), Some(List(11, 5, 8, 11, 19, 16, 1, 14, 8, 18, 4, 11, 16, 8, 10, 17, 6, 2, 16, 13)), None, Some(List(9, 7, 1, 7, 9, 4, 14, 20, 11, 10, 14, 14, 6, 4)), Some(List(11, 5, 8, 11, 19, 16, 1, 14, 8, 18, 4, 11, 16, 8, 10, 17, 6, 2, 16, 13)), Some(List(5, 5, 4, 1, 15, 3, 15, 16, 2, 11, 6, 19, 5, 20, 2, 6)), Some(List(5, 5, 4, 1, 15, 3, 15, 16, 2, 11, 6, 19, 5, 20, 2, 6)), Some(List(1, 13, 7, 6, 17, 7, 20, 8, 12, 5, 5, 9, 6, 18, 14, 1, 9, 8, 18, 5)), Some(List(4, 1, 18, 8, 19, 2, 20, 12, 20, 9, 4, 5, 10, 1, 10, 16, 15, 6)), None, Some(List(12, 16, 11, 11, 5, 14, 11, 4, 8, 19, 11, 5, 3, 13, 15, 7)), None, Some(List(1, 14, 2, 5, 3, 2, 10, 18, 10, 9, 13, 5, 11, 10, 19, 19, 16, 18)), None, Some(List(18, 5, 1, 17, 12, 11, 8, 7, 2, 11, 17, 10, 10, 2, 6, 19, 9)), Some(List(11, 16, 2, 20, 15, 13, 3, 13, 8, 19)), Some(List(14, 7, 3, 16, 17, 11, 14, 14, 19, 4, 9, 14, 9, 13, 13, 15)), Some(List(15, 12, 15, 5, 14, 7, 14, 3, 2, 17, 8, 13, 17, 13, 19, 6, 14, 12, 16)), None, None, Some(List(16, 19, 9, 20, 15, 1, 13, 3, 19, 14, 1, 18)), Some(List(16, 12, 5, 8, 20, 9, 7, 7, 17, 4, 2, 18, 13, 2, 18, 19, 11)), Some(List(2, 6, 11, 18, 9, 3, 9, 15, 14, 11, 11, 20, 5, 16)), Some(List(4, 12, 12, 5, 4, 14, 14, 11, 18, 19)), None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
          val seq5 = List(Some(List(5, 11, 2, 14, 13, 2, 3, 15, 10, 15, 10, 13, 5, 9, 4)), None, Some(List(10, 13, 19, 7, 20, 15, 16, 12, 19, 6, 5)), Some(List(5, 11, 2, 14, 13, 2, 3, 15, 10, 15, 10, 13, 5, 9, 4)), None, None, Some(List(11, 18, 14, 9, 7, 20, 13, 7, 12, 18, 4)), Some(List(20, 16, 17, 4, 11, 8, 17, 12, 11, 9, 19, 9, 14, 7, 10, 20, 7, 5)), Some(List(3, 14, 10, 6, 11, 19, 12, 7, 19, 16, 5, 6, 2, 8, 9, 7, 14, 17)), None, Some(List(4, 13, 12, 14, 1, 7, 6, 15, 2, 11, 15, 19, 14, 3)), Some(List(12, 4, 1, 1, 3, 12, 4, 12, 19, 8, 2, 11, 7, 19, 18, 14, 14)), None, Some(List(1, 5, 13, 11, 15, 20, 1, 17, 6, 16, 14, 8, 2, 17, 9, 11, 14, 9)), Some(List(3, 14, 10, 6, 11, 19, 12, 7, 19, 16, 5, 6, 2, 8, 9, 7, 14, 17)), Some(List(15, 9, 13, 19, 5, 19, 19, 2, 17, 10, 11, 2, 16, 11, 3)), Some(List(15, 19, 3, 19, 7, 6, 9, 3, 8, 10, 5, 2, 14, 9, 15, 6, 16, 5, 10)), Some(List(8, 20, 10, 6, 1, 7, 2, 14, 17, 3, 6, 16, 20, 6, 16)), None, Some(List(8, 16, 18, 7, 1, 10, 15, 12, 15, 17, 15, 4, 6, 6, 1, 9)), Some(List(19, 19, 10, 5, 19, 7, 9, 14, 15, 12, 7, 3, 8, 10)), None, Some(List(19, 19, 10, 5, 19, 7, 9, 14, 15, 12, 7, 3, 8, 10)), None, None, Some(List(14, 6, 20, 19, 19, 18, 4, 6, 20, 18)), None, None, None, Some(List(12, 4, 1, 1, 3, 12, 4, 12, 19, 8, 2, 11, 7, 19, 18, 14, 14)), Some(List(17, 6, 11, 6, 6, 16, 5, 7, 20, 10, 4, 14, 11, 8, 20, 17, 1, 8, 11, 16)), Some(List(5, 1, 20, 13, 18, 10, 2, 17, 8, 2, 8)), Some(List(20, 19, 11, 12, 5, 2, 13, 13, 15, 3, 9, 7, 15, 15)), Some(List(16, 9, 11, 2, 15, 3, 18, 17, 10, 2, 16)), Some(List(4, 13, 12, 14, 1, 7, 6, 15, 2, 11, 15, 19, 14, 3)), None, Some(List(8, 11, 20, 18, 16, 10, 2, 7, 19, 13, 11, 19, 2, 11, 17, 3, 2, 16, 13)), None, Some(List(17, 16, 1, 16, 20, 12, 15, 3, 9, 15, 14, 10, 3, 8, 12, 18, 6, 19)), Some(List(20, 16, 17, 4, 11, 8, 17, 12, 11, 9, 19, 9, 14, 7, 10, 20, 7, 5)), None, None, None, None, Some(List(17, 6, 11, 6, 6, 16, 5, 7, 20, 10, 4, 14, 11, 8, 20, 17, 1, 8, 11, 16)), None, Some(List(18, 6, 6, 19, 12, 13, 16, 10, 1, 13, 19)), Some(List(5, 1, 20, 13, 18, 10, 2, 17, 8, 2, 8)), Some(List(7, 18, 20, 8, 8, 12, 4, 11, 6, 18, 17, 9, 19, 13, 12, 17, 18, 16)), Some(List(20, 9, 20, 17, 3, 12, 16, 20, 5, 8, 6, 3, 10, 4, 13, 15, 6, 14)), None, Some(List(8, 11, 20, 18, 16, 10, 2, 7, 19, 13, 11, 19, 2, 11, 17, 3, 2, 16, 13)), Some(List(13, 6, 3, 10, 2, 11, 4, 17, 2, 9, 13, 3, 1)), None, None, None, None, None, Some(List(1, 16, 20, 6, 2, 14, 5, 15, 19, 12, 8, 9)), Some(List(15, 3, 8, 11, 1, 12, 6, 10, 3, 9, 9)), None, Some(List(9, 1, 18, 15, 19, 10, 5, 12, 11, 7, 5, 2, 4, 15, 16, 12, 8, 10)), Some(List(19, 1, 19, 14, 4, 17, 13, 14, 5, 2, 3, 14, 13, 4, 7, 12, 13, 6, 7)), Some(List(12, 15, 3, 10, 10, 18, 3, 1, 11, 7)), None, None, Some(List(15, 3, 8, 11, 1, 12, 6, 10, 3, 9, 9)), None, Some(List(17, 12, 10, 14, 15, 19, 18, 8, 20, 20)), None, None, Some(List(1, 9, 8, 5, 7, 18, 2, 12, 12, 14, 6, 12, 19)), Some(List(20, 9, 20, 17, 3, 12, 16, 20, 5, 8, 6, 3, 10, 4, 13, 15, 6, 14)), None, None, Some(List(19, 6, 3, 17, 3, 18, 20, 20, 11, 2, 12, 3, 19, 15, 3, 14, 16, 2, 6)), None, Some(List(14, 6, 20, 19, 19, 18, 4, 6, 20, 18)), Some(List(12, 15, 3, 10, 10, 18, 3, 1, 11, 7)), Some(List(17, 6, 3, 9, 10, 11, 16, 18, 16, 6)), Some(List(20, 2, 14, 17, 17, 8, 11, 3, 6, 7, 11, 18, 11)), None, None, Some(List(4, 3, 8, 7, 16, 15, 12, 2, 15, 20, 4, 16, 15)), None, Some(List(14, 3, 11, 19, 15, 8, 4, 14, 4, 15, 20, 6, 3, 20)), None, Some(List(6, 6, 2, 5, 2, 9, 12, 19, 7, 6)), None, Some(List(14, 3, 11, 19, 15, 8, 4, 14, 4, 15, 20, 6, 3, 20)), None, Some(List(9, 3, 11, 4, 20, 15, 17, 4, 13, 17)), None, None, Some(List(15, 7, 3, 5, 20, 17, 3, 18, 14, 12, 1, 5, 3, 3, 11)), None, None, None, Some(List(14, 10, 14, 7, 9, 10, 1, 17, 9, 16, 4, 9, 15, 8)), Some(List(6, 13, 6, 19, 2, 9, 4, 12, 12, 16, 11, 8)), Some(List(15, 16, 12, 11, 19, 13, 1, 8, 8, 9, 18, 10, 7)), None, Some(List(8, 11, 20, 18, 16, 10, 2, 7, 19, 13, 11, 19, 2, 11, 17, 3, 2, 16, 13)), Some(List(20, 9, 20, 17, 3, 12, 16, 20, 5, 8, 6, 3, 10, 4, 13, 15, 6, 14)), Some(List(14, 6, 20, 19, 19, 18, 4, 6, 20, 18)), Some(List(1, 16, 20, 6, 2, 14, 5, 15, 19, 12, 8, 9)), None, Some(List(12, 17, 9, 3, 19, 17, 4, 17, 15, 18, 4, 11, 6, 14, 6, 7, 6, 2, 16, 6)), None, None, None, None, Some(List(7, 18, 20, 8, 8, 12, 4, 11, 6, 18, 17, 9, 19, 13, 12, 17, 18, 16)), None, Some(List(7, 14, 4, 3, 18, 17, 16, 13, 6, 11, 20, 3, 20, 2, 16, 5, 8, 4)), None, None, Some(List(12, 10, 6, 13, 9, 20, 18, 20, 1, 16, 17, 9, 2, 20, 9, 14)), None, None, Some(List(9, 20, 9, 20, 8, 8, 10, 9, 1, 17)), None, None, Some(List(19, 6, 3, 17, 3, 18, 20, 20, 11, 2, 12, 3, 19, 15, 3, 14, 16, 2, 6)), Some(List(14, 10, 14, 7, 9, 10, 1, 17, 9, 16, 4, 9, 15, 8)), Some(List(8, 16, 18, 7, 1, 10, 15, 12, 15, 17, 15, 4, 6, 6, 1, 9)), Some(List(12, 15, 3, 10, 10, 18, 3, 1, 11, 7)), None, Some(List(4, 13, 12, 14, 1, 7, 6, 15, 2, 11, 15, 19, 14, 3)), None, None, None, None, Some(List(1, 9, 8, 5, 7, 18, 2, 12, 12, 14, 6, 12, 19)), None, Some(List(19, 4, 17, 10, 1, 20, 17, 10, 2, 7, 19, 6, 10, 18, 17, 8, 9, 11, 12)), Some(List(7, 8, 11, 7, 9, 12, 14, 13, 10, 5, 18, 13, 6, 15)), Some(List(20, 19, 11, 12, 5, 2, 13, 13, 15, 3, 9, 7, 15, 15)), Some(List(15, 3, 8, 11, 1, 12, 6, 10, 3, 9, 9)), Some(List(19, 4, 17, 10, 1, 20, 17, 10, 2, 7, 19, 6, 10, 18, 17, 8, 9, 11, 12)), Some(List(15, 3, 8, 11, 1, 12, 6, 10, 3, 9, 9)), None, Some(List(9, 1, 18, 15, 19, 10, 5, 12, 11, 7, 5, 2, 4, 15, 16, 12, 8, 10)), None, Some(List(19, 5, 3, 6, 16, 10, 2, 9, 6, 14, 2, 15, 1, 1, 17, 6, 7)), Some(List(6, 6, 2, 5, 2, 9, 12, 19, 7, 6)), None, None, None, Some(List(15, 19, 3, 19, 7, 6, 9, 3, 8, 10, 5, 2, 14, 9, 15, 6, 16, 5, 10)), Some(List(16, 13, 14, 3, 10, 12, 9, 13, 13, 19, 2, 9)), None, Some(List(17, 5, 16, 13, 11, 16, 17, 15, 8, 14, 4, 6, 13, 19, 6, 11, 15, 9, 1, 5)), Some(List(6, 5, 19, 7, 15, 4, 8, 16, 6, 15, 16, 1)), Some(List(8, 20, 10, 6, 1, 7, 2, 14, 17, 3, 6, 16, 20, 6, 16)), Some(List(20, 8, 14, 7, 16, 15, 20, 11, 16, 13, 4, 13, 7, 11)), Some(List(7, 19, 9, 2, 20, 8, 19, 11, 7, 18, 13, 6)), Some(List(15, 10, 20, 19, 10, 13, 10, 14, 15, 6, 15, 13, 20, 19, 15)), Some(List(5, 6, 19, 7, 6, 15, 2, 1, 5, 6, 15, 17, 3)), Some(List(2, 7, 8, 2, 18, 10, 9, 15, 2, 19)), Some(List(19, 1, 19, 14, 4, 17, 13, 14, 5, 2, 3, 14, 13, 4, 7, 12, 13, 6, 7)), Some(List(8, 20, 10, 6, 1, 7, 2, 14, 17, 3, 6, 16, 20, 6, 16)), Some(List(3, 17, 19, 6, 3, 12, 2, 1, 9, 20, 10, 20, 19, 7, 20, 4, 7, 4)), Some(List(17, 6, 3, 9, 10, 11, 16, 18, 16, 6)), Some(List(19, 19, 15, 19, 19, 19, 17, 9, 1, 20, 10, 1, 18, 16)), Some(List(17, 2, 3, 7, 3, 19, 8, 2, 5, 2, 3)), Some(List(15, 12, 5, 5, 2, 8, 11, 7, 13, 7, 9, 18, 1, 12, 12, 10, 15)), Some(List(20, 8, 14, 7, 16, 15, 20, 11, 16, 13, 4, 13, 7, 11)), None, Some(List(14, 3, 11, 19, 15, 8, 4, 14, 4, 15, 20, 6, 3, 20)), Some(List(19, 5, 3, 6, 16, 10, 2, 9, 6, 14, 2, 15, 1, 1, 17, 6, 7)), Some(List(14, 11, 7, 14, 1, 16, 17, 16, 12, 3, 15, 12, 6, 18, 15, 19, 3, 13, 3)), None, None, Some(List(17, 16, 1, 16, 20, 12, 15, 3, 9, 15, 14, 10, 3, 8, 12, 18, 6, 19)), Some(List(1, 5, 13, 5, 8, 6, 8, 17, 6, 19, 15)), None, Some(List(14, 3, 11, 19, 15, 8, 4, 14, 4, 15, 20, 6, 3, 20)), Some(List(15, 19, 3, 19, 7, 6, 9, 3, 8, 10, 5, 2, 14, 9, 15, 6, 16, 5, 10)), Some(List(17, 2, 3, 7, 3, 19, 8, 2, 5, 2, 3)), Some(List(2, 7, 8, 2, 18, 10, 9, 15, 2, 19)), Some(List(17, 9, 8, 8, 3, 12, 9, 17, 8, 9, 1, 1, 14, 14, 19)), Some(List(8, 16, 18, 7, 1, 10, 15, 12, 15, 17, 15, 4, 6, 6, 1, 9)), Some(List(7, 19, 9, 2, 20, 8, 19, 11, 7, 18, 13, 6)), None, None, None, None, None, None, None, None, Some(List(17, 5, 16, 13, 11, 16, 17, 15, 8, 14, 4, 6, 13, 19, 6, 11, 15, 9, 1, 5)), None, None, Some(List(3, 14, 10, 6, 11, 19, 12, 7, 19, 16, 5, 6, 2, 8, 9, 7, 14, 17)), Some(List(9, 3, 11, 4, 20, 15, 17, 4, 13, 17)), Some(List(12, 10, 6, 13, 9, 20, 18, 20, 1, 16, 17, 9, 2, 20, 9, 14)), Some(List(15, 19, 3, 19, 7, 6, 9, 3, 8, 10, 5, 2, 14, 9, 15, 6, 16, 5, 10)), Some(List(7, 19, 9, 2, 20, 8, 19, 11, 7, 18, 13, 6)), Some(List(12, 10, 6, 13, 9, 20, 18, 20, 1, 16, 17, 9, 2, 20, 9, 14)), None, Some(List(4, 7, 12, 3, 19, 14, 7, 8, 19, 20, 6, 13, 9, 17)), Some(List(3, 17, 19, 6, 3, 12, 2, 1, 9, 20, 10, 20, 19, 7, 20, 4, 7, 4)), None, Some(List(14, 4, 9, 12, 5, 2, 8, 9, 2, 20, 19, 7, 19, 16, 5, 18, 20, 3, 5)), Some(List(15, 19, 3, 19, 7, 6, 9, 3, 8, 10, 5, 2, 14, 9, 15, 6, 16, 5, 10)), Some(List(7, 8, 11, 7, 9, 12, 14, 13, 10, 5, 18, 13, 6, 15)), Some(List(9, 13, 18, 12, 19, 16, 16, 19, 7, 3, 3, 13, 15)), Some(List(8, 19, 14, 19, 18, 20, 18, 2, 3, 18, 8)), Some(List(12, 15, 3, 10, 10, 18, 3, 1, 11, 7)), None, Some(List(4, 4, 5, 14, 19, 10, 6, 20, 18, 12, 10, 1)), None, None, None, None, Some(List(9, 13, 18, 12, 19, 16, 16, 19, 7, 3, 3, 13, 15)), None, None, None, None, None, Some(List(13, 6, 3, 10, 2, 11, 4, 17, 2, 9, 13, 3, 1)), Some(List(5, 6, 19, 7, 6, 15, 2, 1, 5, 6, 15, 17, 3)), Some(List(15, 9, 13, 19, 5, 19, 19, 2, 17, 10, 11, 2, 16, 11, 3)), None, None, Some(List(12, 8, 3, 8, 19, 3, 18, 11, 19, 5, 19, 9, 12, 14)), None, Some(List(19, 19, 10, 5, 19, 7, 9, 14, 15, 12, 7, 3, 8, 10)), Some(List(10, 13, 19, 7, 20, 15, 16, 12, 19, 6, 5)), Some(List(15, 3, 8, 11, 1, 12, 6, 10, 3, 9, 9)), None, Some(List(16, 9, 11, 2, 15, 3, 18, 17, 10, 2, 16)), Some(List(9, 3, 11, 4, 20, 15, 17, 4, 13, 17)), None, Some(List(16, 9, 11, 2, 15, 3, 18, 17, 10, 2, 16)), Some(List(9, 20, 9, 20, 8, 8, 10, 9, 1, 17)), Some(List(20, 8, 14, 7, 16, 15, 20, 11, 16, 13, 4, 13, 7, 11)), None, Some(List(17, 12, 10, 14, 15, 19, 18, 8, 20, 20)), Some(List(4, 7, 12, 3, 19, 14, 7, 8, 19, 20, 6, 13, 9, 17)), Some(List(15, 10, 20, 19, 10, 13, 10, 14, 15, 6, 15, 13, 20, 19, 15)), None, Some(List(10, 13, 18, 18, 3, 9, 17, 19, 18, 20)), Some(List(19, 4, 17, 10, 1, 20, 17, 10, 2, 7, 19, 6, 10, 18, 17, 8, 9, 11, 12)), None, Some(List(12, 13, 12, 16, 10, 9, 18, 18, 11, 2, 13, 12, 7)), Some(List(3, 17, 19, 6, 3, 12, 2, 1, 9, 20, 10, 20, 19, 7, 20, 4, 7, 4)), Some(List(14, 3, 11, 19, 15, 8, 4, 14, 4, 15, 20, 6, 3, 20)), Some(List(1, 14, 15, 12, 17, 7, 12, 17, 16, 15, 7, 12, 5, 10, 5, 9, 7)), Some(List(1, 5, 13, 11, 15, 20, 1, 17, 6, 16, 14, 8, 2, 17, 9, 11, 14, 9)), None, Some(List(5, 11, 2, 14, 13, 2, 3, 15, 10, 15, 10, 13, 5, 9, 4)), Some(List(7, 18, 20, 8, 8, 12, 4, 11, 6, 18, 17, 9, 19, 13, 12, 17, 18, 16)), None, None, None, Some(List(19, 19, 15, 19, 19, 19, 17, 9, 1, 20, 10, 1, 18, 16)), None, Some(List(9, 3, 11, 4, 20, 15, 17, 4, 13, 17)), Some(List(10, 1, 3, 7, 6, 4, 16, 13, 12, 11, 14, 1, 15, 12, 13, 9, 4)), None, Some(List(17, 6, 3, 9, 10, 11, 16, 18, 16, 6)), Some(List(19, 19, 15, 19, 19, 19, 17, 9, 1, 20, 10, 1, 18, 16)), None, None, Some(List(19, 4, 17, 10, 1, 20, 17, 10, 2, 7, 19, 6, 10, 18, 17, 8, 9, 11, 12)), Some(List(1, 14, 15, 12, 17, 7, 12, 17, 16, 15, 7, 12, 5, 10, 5, 9, 7)), Some(List(16, 14, 10, 3, 8, 19, 3, 17, 14, 17, 3, 14, 12, 16)), Some(List(9, 5, 13, 10, 7, 7, 15, 18, 19, 13, 18, 14, 9, 10, 12, 20)), Some(List(9, 13, 18, 12, 19, 16, 16, 19, 7, 3, 3, 13, 15)), Some(List(15, 10, 20, 19, 10, 13, 10, 14, 15, 6, 15, 13, 20, 19, 15)), Some(List(4, 13, 9, 16, 16, 8, 13, 10, 7, 19, 2, 19, 17)), Some(List(12, 17, 9, 3, 19, 17, 4, 17, 15, 18, 4, 11, 6, 14, 6, 7, 6, 2, 16, 6)), None, None, Some(List(6, 12, 12, 12, 6, 14, 12, 12, 5, 11, 6, 4, 4, 10)), Some(List(13, 6, 3, 10, 2, 11, 4, 17, 2, 9, 13, 3, 1)), None, None, Some(List(6, 4, 1, 8, 1, 13, 17, 5, 9, 9, 6, 4, 4, 8, 13, 14, 2, 20)), Some(List(17, 6, 3, 9, 10, 11, 16, 18, 16, 6)), Some(List(18, 14, 3, 16, 5, 2, 8, 17, 3, 13, 11, 17, 18, 9, 11, 19, 6, 18)), None, Some(List(20, 19, 11, 12, 5, 2, 13, 13, 15, 3, 9, 7, 15, 15)), None, Some(List(6, 4, 1, 8, 1, 13, 17, 5, 9, 9, 6, 4, 4, 8, 13, 14, 2, 20)), None, Some(List(8, 20, 10, 6, 1, 7, 2, 14, 17, 3, 6, 16, 20, 6, 16)), None, Some(List(17, 19, 18, 5, 2, 18, 19, 1, 3, 7, 3, 6, 18, 11, 19, 16, 11, 16)), None, None, Some(List(7, 18, 20, 8, 8, 12, 4, 11, 6, 18, 17, 9, 19, 13, 12, 17, 18, 16)), None, Some(List(5, 1, 20, 13, 18, 10, 2, 17, 8, 2, 8)), Some(List(11, 20, 15, 16, 17, 1, 11, 17, 8, 7, 11, 2, 2, 17, 11, 4, 8, 12)), Some(List(9, 5, 13, 10, 7, 7, 15, 18, 19, 13, 18, 14, 9, 10, 12, 20)), None, None, Some(List(8, 11, 20, 18, 16, 10, 2, 7, 19, 13, 11, 19, 2, 11, 17, 3, 2, 16, 13)), Some(List(14, 10, 14, 7, 9, 10, 1, 17, 9, 16, 4, 9, 15, 8)), Some(List(1, 16, 20, 6, 2, 14, 5, 15, 19, 12, 8, 9)), Some(List(20, 14, 7, 7, 14, 2, 12, 4, 11, 20)), None, None, Some(List(9, 13, 18, 12, 19, 16, 16, 19, 7, 3, 3, 13, 15)), Some(List(14, 11, 7, 14, 1, 16, 17, 16, 12, 3, 15, 12, 6, 18, 15, 19, 3, 13, 3)), None, Some(List(15, 9, 13, 19, 5, 19, 19, 2, 17, 10, 11, 2, 16, 11, 3)), Some(List(15, 3, 8, 11, 1, 12, 6, 10, 3, 9, 9)), None, None, Some(List(14, 10, 14, 7, 9, 10, 1, 17, 9, 16, 4, 9, 15, 8)), Some(List(20, 16, 17, 4, 11, 8, 17, 12, 11, 9, 19, 9, 14, 7, 10, 20, 7, 5)), Some(List(12, 10, 6, 13, 9, 20, 18, 20, 1, 16, 17, 9, 2, 20, 9, 14)), Some(List(6, 10, 12, 1, 11, 19, 20, 7, 10, 3, 10, 5, 18, 13)), None, Some(List(12, 4, 1, 1, 3, 12, 4, 12, 19, 8, 2, 11, 7, 19, 18, 14, 14)), None, Some(List(10, 13, 19, 7, 20, 15, 16, 12, 19, 6, 5)), Some(List(16, 9, 11, 2, 15, 3, 18, 17, 10, 2, 16)), None, None, None, Some(List(15, 16, 12, 11, 19, 13, 1, 8, 8, 9, 18, 10, 7)), None, None, Some(List(8, 16, 18, 7, 1, 10, 15, 12, 15, 17, 15, 4, 6, 6, 1, 9)), Some(List(4, 13, 12, 14, 1, 7, 6, 15, 2, 11, 15, 19, 14, 3)), Some(List(7, 18, 20, 8, 8, 12, 4, 11, 6, 18, 17, 9, 19, 13, 12, 17, 18, 16)), None, Some(List(9, 20, 9, 20, 8, 8, 10, 9, 1, 17)), None, None, Some(List(7, 8, 11, 7, 9, 12, 14, 13, 10, 5, 18, 13, 6, 15)), Some(List(8, 11, 20, 18, 16, 10, 2, 7, 19, 13, 11, 19, 2, 11, 17, 3, 2, 16, 13)), None, None, None, None, Some(List(14, 4, 9, 12, 5, 2, 8, 9, 2, 20, 19, 7, 19, 16, 5, 18, 20, 3, 5)), Some(List(8, 13, 19, 17, 14, 16, 12, 15, 5, 18, 8, 6)), None, Some(List(12, 15, 3, 10, 10, 18, 3, 1, 11, 7)), None, Some(List(4, 4, 5, 14, 19, 10, 6, 20, 18, 12, 10, 1)), None, Some(List(12, 17, 9, 3, 19, 17, 4, 17, 15, 18, 4, 11, 6, 14, 6, 7, 6, 2, 16, 6)), Some(List(11, 20, 15, 16, 17, 1, 11, 17, 8, 7, 11, 2, 2, 17, 11, 4, 8, 12)), None, None, Some(List(17, 6, 11, 6, 6, 16, 5, 7, 20, 10, 4, 14, 11, 8, 20, 17, 1, 8, 11, 16)), None, None, None, Some(List(17, 2, 3, 7, 3, 19, 8, 2, 5, 2, 3)), None, None, None, Some(List(15, 16, 12, 11, 19, 13, 1, 8, 8, 9, 18, 10, 7)), Some(List(11, 18, 14, 9, 7, 20, 13, 7, 12, 18, 4)), Some(List(4, 13, 12, 14, 1, 7, 6, 15, 2, 11, 15, 19, 14, 3)), Some(List(19, 19, 15, 19, 19, 19, 17, 9, 1, 20, 10, 1, 18, 16)), None, Some(List(9, 20, 9, 20, 8, 8, 10, 9, 1, 17)), None, Some(List(18, 6, 6, 19, 12, 13, 16, 10, 1, 13, 19)), None, None, None, Some(List(12, 13, 1, 16, 9, 10, 10, 20, 9, 17, 6, 16, 20, 9, 15, 12)), None, None, None, None, None, None, Some(List(5, 1, 20, 13, 18, 10, 2, 17, 8, 2, 8)), Some(List(7, 14, 4, 3, 18, 17, 16, 13, 6, 11, 20, 3, 20, 2, 16, 5, 8, 4)), None, None, None, None, None, Some(List(18, 14, 3, 16, 5, 2, 8, 17, 3, 13, 11, 17, 18, 9, 11, 19, 6, 18)), Some(List(3, 17, 19, 6, 3, 12, 2, 1, 9, 20, 10, 20, 19, 7, 20, 4, 7, 4)), Some(List(17, 9, 7, 6, 20, 11, 11, 15, 14, 17, 17, 17, 16, 17, 7, 6)), None, None, None, Some(List(17, 6, 3, 9, 10, 11, 16, 18, 16, 6)), Some(List(15, 9, 13, 19, 5, 19, 19, 2, 17, 10, 11, 2, 16, 11, 3)), Some(List(17, 9, 8, 8, 3, 12, 9, 17, 8, 9, 1, 1, 14, 14, 19)), Some(List(10, 13, 18, 18, 3, 9, 17, 19, 18, 20)), Some(List(19, 17, 15, 3, 5, 3, 12, 12, 12, 3, 8, 1, 7, 13)), Some(List(15, 19, 3, 19, 7, 6, 9, 3, 8, 10, 5, 2, 14, 9, 15, 6, 16, 5, 10)), Some(List(5, 10, 16, 17, 16, 13, 2, 12, 5, 5, 19, 8, 2, 14)), Some(List(4, 4, 5, 14, 19, 10, 6, 20, 18, 12, 10, 1)), Some(List(12, 4, 1, 1, 3, 12, 4, 12, 19, 8, 2, 11, 7, 19, 18, 14, 14)), None, Some(List(9, 20, 9, 20, 8, 8, 10, 9, 1, 17)), None, Some(List(3, 17, 19, 6, 3, 12, 2, 1, 9, 20, 10, 20, 19, 7, 20, 4, 7, 4)), None, None, None, None, Some(List(19, 19, 15, 19, 19, 19, 17, 9, 1, 20, 10, 1, 18, 16)), None, None, None, None, Some(List(17, 19, 18, 5, 2, 18, 19, 1, 3, 7, 3, 6, 18, 11, 19, 16, 11, 16)), Some(List(17, 9, 7, 6, 20, 11, 11, 15, 14, 17, 17, 17, 16, 17, 7, 6)), Some(List(9, 3, 11, 4, 20, 15, 17, 4, 13, 17)), None, None, Some(List(12, 4, 1, 1, 3, 12, 4, 12, 19, 8, 2, 11, 7, 19, 18, 14, 14)), Some(List(6, 12, 12, 12, 6, 14, 12, 12, 5, 11, 6, 4, 4, 10)), None, Some(List(15, 8, 15, 14, 3, 2, 13, 18, 20, 7, 5)), Some(List(5, 10, 1, 6, 5, 20, 3, 3, 5, 6, 3)), Some(List(7, 19, 9, 2, 20, 8, 19, 11, 7, 18, 13, 6)), Some(List(20, 2, 14, 17, 17, 8, 11, 3, 6, 7, 11, 18, 11)), Some(List(16, 13, 14, 3, 10, 12, 9, 13, 13, 19, 2, 9)), None, Some(List(4, 4, 5, 14, 19, 10, 6, 20, 18, 12, 10, 1)), Some(List(1, 16, 20, 6, 2, 14, 5, 15, 19, 12, 8, 9)), None, None, None, None, Some(List(9, 20, 9, 20, 8, 8, 10, 9, 1, 17)), None, Some(List(17, 9, 7, 6, 20, 11, 11, 15, 14, 17, 17, 17, 16, 17, 7, 6)), Some(List(12, 13, 12, 16, 10, 9, 18, 18, 11, 2, 13, 12, 7)), None, None, Some(List(17, 19, 18, 5, 2, 18, 19, 1, 3, 7, 3, 6, 18, 11, 19, 16, 11, 16)), Some(List(4, 3, 8, 7, 16, 15, 12, 2, 15, 20, 4, 16, 15)), None, Some(List(14, 3, 11, 19, 15, 8, 4, 14, 4, 15, 20, 6, 3, 20)), None, Some(List(13, 16, 7, 9, 6, 20, 12, 16, 9, 11)), None, Some(List(8, 13, 19, 17, 14, 16, 12, 15, 5, 18, 8, 6)), None, None, Some(List(6, 6, 2, 5, 2, 9, 12, 19, 7, 6)), Some(List(7, 18, 20, 8, 8, 12, 4, 11, 6, 18, 17, 9, 19, 13, 12, 17, 18, 16)), Some(List(4, 7, 12, 3, 19, 14, 7, 8, 19, 20, 6, 13, 9, 17)), Some(List(8, 20, 10, 6, 1, 7, 2, 14, 17, 3, 6, 16, 20, 6, 16)), Some(List(4, 13, 12, 14, 1, 7, 6, 15, 2, 11, 15, 19, 14, 3)), Some(List(7, 14, 4, 3, 18, 17, 16, 13, 6, 11, 20, 3, 20, 2, 16, 5, 8, 4)), None, None, None, Some(List(20, 9, 20, 17, 3, 12, 16, 20, 5, 8, 6, 3, 10, 4, 13, 15, 6, 14)), None, None, Some(List(17, 6, 3, 9, 10, 11, 16, 18, 16, 6)), Some(List(6, 5, 19, 7, 15, 4, 8, 16, 6, 15, 16, 1)), None, Some(List(10, 18, 1, 6, 1, 6, 6, 16, 14, 6, 5)), Some(List(1, 4, 1, 15, 6, 16, 15, 1, 16, 20)), Some(List(7, 8, 11, 7, 9, 12, 14, 13, 10, 5, 18, 13, 6, 15)), None, None, None, None, Some(List(17, 9, 7, 6, 20, 11, 11, 15, 14, 17, 17, 17, 16, 17, 7, 6)), Some(List(12, 8, 3, 8, 19, 3, 18, 11, 19, 5, 19, 9, 12, 14)), Some(List(17, 6, 3, 9, 10, 11, 16, 18, 16, 6)), None, Some(List(20, 16, 17, 4, 11, 8, 17, 12, 11, 9, 19, 9, 14, 7, 10, 20, 7, 5)), None, Some(List(9, 3, 11, 4, 20, 15, 17, 4, 13, 17)), None, None, None, None, Some(List(8, 19, 14, 19, 18, 20, 18, 2, 3, 18, 8)), Some(List(12, 10, 6, 13, 9, 20, 18, 20, 1, 16, 17, 9, 2, 20, 9, 14)), Some(List(17, 12, 10, 14, 15, 19, 18, 8, 20, 20)), None, Some(List(6, 4, 1, 8, 1, 13, 17, 5, 9, 9, 6, 4, 4, 8, 13, 14, 2, 20)), Some(List(15, 16, 12, 11, 19, 13, 1, 8, 8, 9, 18, 10, 7)), None, None, Some(List(19, 19, 15, 19, 19, 19, 17, 9, 1, 20, 10, 1, 18, 16)), None, Some(List(7, 18, 20, 8, 8, 12, 4, 11, 6, 18, 17, 9, 19, 13, 12, 17, 18, 16)), None, None, Some(List(7, 14, 4, 3, 18, 17, 16, 13, 6, 11, 20, 3, 20, 2, 16, 5, 8, 4)), None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
         Q2.countAcc(seq) mustEqual 6
         Q2.countAcc(seq2) mustEqual 12
         Q2.countAcc(seq3) mustEqual 18
         Q2.countAcc(seq4) mustEqual 81
         Q2.countAcc(seq5) mustEqual 828
         Q2.countAcc(seqUltimate) mustEqual 161
         6 mustEqual 6
      }
   }



   "Maze solution" should {

    "Find solution" in {



      def sparsaj(ime:String):(Int, Int, List[Boolean]) = {
        val lines = Source.fromFile(ime).getLines.toList
        def toLinear(l:List[String]): List[Char] = l.foldLeft(List[Char]())((acc, c) => c.toList.reverse ::: acc)
        def strToBoolean(l:List[Char]):List[Boolean] = l.foldLeft(List[Boolean]())((acc, c) => if(c == '*')  acc.::(false) else acc.::(true))
        (lines.size, lines(0).size, strToBoolean(toLinear(lines)))  

      }

      val maze1 = sparsaj("exampleMazes/maze1.txt")
      val sirina = 31
      val visina = 21
      maze1._2 mustEqual 31
      maze1._3.size mustEqual 651
      //val maze = new Maze(maze1._1, maze1._2, maze1._3)

      //maze.solution mustEqual Nil



    }
   }

   //testi za Q2

   //testi za labirint






   //Source.fromFile("_____").getLines.toList

}
