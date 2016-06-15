import org.specs2.mutable._

class Week3Tests extends Specification {
  val solution = new Main


  "the list functions" should {
  	"get ith element in a list" in {
  		solution.get(2,List(1,2,3)) mustEqual 3
  		solution.get(5,List(1,2,3)) mustEqual 3
  		solution.get(0,List(5,4,3)) mustEqual 5
  		solution.get(2, List(1,2,3,4)) mustEqual 3
  	}

  	"find given element in a list" in {
  		solution.find(5,List(1,2,3)) mustEqual -1
  		solution.find(2,List(1,2,3)) mustEqual 1
  		solution.find(5,List()) mustEqual -1
  		solution.find(5, List(5)) mustEqual 0
  	}

  	"delete ith element from a list" in {
  		solution.delete(3,List(1,2,3,4,5)) mustEqual List(1,2,3,5)
  		solution.delete(0,List(1)) mustEqual Nil
  		solution.delete(0,List()) mustEqual Nil
  		solution.delete(5,List(1,2)) mustEqual List(1,2)
  	}

  	"concat two lists" in {
  		solution.concat(List(1,2), List(3,4)) mustEqual List(1,2,3,4)
  		solution.concat(Nil,Nil) mustEqual Nil
  		solution.concat(Nil, List(1,2)) mustEqual List(1,2)
  		solution.concat(List(1,2), Nil) mustEqual List(1,2)
  		solution.concat(List(1,3,5,7), List(2,4,6)) mustEqual List(1,3,5,7,2,4,6)
  	}
  	"sum two lists" in {
  		solution.zipperSum(List(1,2,3),List(1,2,3,4)) mustEqual List(2,4,6)
  		solution.zipperSum(Nil,Nil) mustEqual Nil
  		solution.zipperSum(List(1,5), Nil) mustEqual Nil
  		solution.zipperSum(Nil,List(5,6)) mustEqual Nil
  		solution.zipperSum(List(1), List(-1)) mustEqual List(0)
  	}

  	"zipper two lists" in {
  		solution.zipper(List(1,3,5,7), List(2,4,6)) mustEqual List(1,2,3,4,5,6,7)
  		solution.zipper(List(1,5,7), List(2,5,10)) mustEqual List(1,2,5,5,7,10)
  		solution.zipper(List(1,3,5),List(2,4,6,8,10)) mustEqual List(1,2,3,4,5,6,8,10)
  		solution.zipper(Nil, List(1,2,3)) mustEqual List(1,2,3)
  		solution.zipper(Nil,Nil) mustEqual Nil
  		solution.zipper(List(1,2,3), Nil) mustEqual List(1,2,3)
  	}

  	"map should apply function to elements in list" in {
  		solution.map(List(1,2,3), x => x+1) mustEqual List(2,3,4)
  		solution.map(Nil, x=>x+1) mustEqual Nil
  		solution.map(List(5), x=>x*2) mustEqual List(10)
  		solution.map(List(1,2,3),x => 1) mustEqual List(1,1,1)
  	}

  	"foldLeft list" in {
  		solution.foldLeft(0,List(1,2,3),(acc,head) => acc+head) mustEqual 6
  		solution.foldLeft(5,List(5), (acc,head) => acc*head) mustEqual 25
  		solution.foldLeft(5,Nil,(acc,head) => 0) mustEqual 5
  		solution.foldLeft(1,List(2,4,6), (acc,head) => acc*head) mustEqual 48
  		solution.foldLeft(5, List(5,6,7), (acc,head) => 0) mustEqual 0

  	}


  }

  "MTF decrypting functions should" should {
  	"properly apply all the functions to encode and decode" in {
      solution.decodeMTF(solution.encodeMTF("")) mustEqual ""
  		solution.decodeMTF(solution.encodeMTF("ma")) mustEqual "ma"
      solution.decodeMTF(solution.encodeMTF("m")) mustEqual "m"
      solution.decodeMTF(solution.encodeMTF("opr")) mustEqual "opr"
      solution.decodeMTF(solution.encodeMTF("abc")) mustEqual "abc"
      solution.decodeMTF(solution.encodeMTF("aaa")) mustEqual "aaa"
      solution.decodeMTF(solution.encodeMTF("broodmother")) mustEqual "broodmother"
      solution.decodeMTF(solution.encodeMTF("malo bolj daljsi najdaljsi test")) mustEqual "malo bolj daljsi najdaljsi test"
  	}




    }

  "MTF advanced decrypting functions" should {
    "properly apply all the functions to do the advanced encryption" in {
      solution.encodeMTF2("mama") mustEqual List(109,98,0)
      solution.encodeMTF2("mamama") mustEqual List(109,98,0,0)
      solution.encodeMTF2("aaaaa") mustEqual List(97,0,0,1)
      solution.encodeMTF2("abab") mustEqual List(97,98,0)
      solution.decodeMTF2(solution.encodeMTF2("mama")) mustEqual "mama"
      solution.decodeMTF2(solution.encodeMTF2("abc")) mustEqual "abc"
      solution.decodeMTF2(solution.encodeMTF2("aaa")) mustEqual "aaa"
      solution.decodeMTF2(solution.encodeMTF2("broodmoother")) mustEqual "broodmoother"
      solution.decodeMTF2(solution.encodeMTF2("malo bolj daljsi najdaljsi test")) mustEqual "malo bolj daljsi najdaljsi test"


      }

  }
  //Napišite teste za osnovne funkcije za delo s seznami


  //Napišite teste za MTF kodiranje


  //Napišite teste za MFT++ kodiranje

}
