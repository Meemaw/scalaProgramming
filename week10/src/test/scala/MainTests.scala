import org.specs2.mutable._
import CSVLang._


class Week10Tests extends Specification {


	def getStream(x:CsvData):Stream[Stream[String]] = x match { case CsvData(x,_) => x }
	def toList(x:Stream[Stream[String]]):List[List[String]] = x.map(x => x.toList).toList


	"Week10Tests" should {

//////////////////////////////////////// VSI TESTI UPORABLAJO TA STRING !
			val string = 
"""a,1,2
b,2,3
"""

////////////////////////////////////////
			val c: CsvData = CSVLang.toCsvData(string)


		"parseToCsvData" in {
			toList(getStream(c)) mustEqual List(List("a", "1", "2"), List("b", "2", "3"))
		}

		"merge CsvData" in {
			toList(getStream(c >>> DataMerge(c))) mustEqual List(List("a", "1", "2"), List("b", "2", "3"),List("a", "1", "2"), List("b", "2", "3"))
		}

		"dropCol" in {
			 toList(getStream(c >>> DropCol(0))) mustEqual List(List("1", "2"), List("2", "3"))
			 toList(getStream(c >>> DropCol(1))) mustEqual List(List("a","2"), List("b", "3"))
		}

		"swapCol" in {
			toList(getStream(c >>> SwapCol(0,1))) mustEqual List(List("1", "a", "2"), List("2", "b", "3"))
			toList(getStream(c >>> SwapCol(0,0))) mustEqual List(List("a", "1", "2"), List("b", "2", "3"))

		}

		"reverseRows" in {
			toList(getStream(c >>> RevAction(true))) mustEqual List(List("2", "1", "a"), List("3", "2", "b"))
		}

		"sequence" in {
			toList(getStream(c >>> DropCol(0) --> SwapCol(0,1))) mustEqual List(List("2", "1"), List("3", "2"))
			toList(getStream(c >>> (drop, 0) --> (swap, 0, 1) --> (out, "test.csv"))) mustEqual List(List("2", "1"), List("3", "2"))
			toList(getStream(c >>> DataMerge(c) --> DropCol(0) --> SwapCol(0,1) --> RevAction(true))) mustEqual List(List("1", "2"), List("2", "3"),List("1", "2"), List("2", "3"))
			toList(getStream(c >>> DataMerge(c) --> (drop,2) --> (drop,1))) mustEqual List(List("a"), List("b"), List("a"), List("b"))
			toList(getStream(("test.csv") >>> revrow --> (drop,0) --> screen)) mustEqual List(List("2"), List("3"))
			toList(getStream(("test.csv") >>> screen)) mustEqual List(List("2", "1"), List("3", "2"))
		}
	}  

}
