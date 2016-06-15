import scala.annotation.tailrec

class Main{

  /*
   * Task 1. Write some basic functions to work with lists
   * Use only head, tail and Nil.
   */


   // Return element on position i in list: get(2, List(1,2,3,4))=3
   def get(i:Int, l:List[Int]):Int = {
    @tailrec def inner(current: Int, reduced: List[Int]): Int = {
      if(current == 0 || reduced.size == 1) reduced.head
      else inner(current-1, reduced.tail)
    }
    inner(i,l)
   }

   def getString(i:Int, l:List[String]):String = {
    @tailrec def inner(current: Int, reduced:List[String]): String = {
      if(current == 0 || reduced.size == 1) reduced.head
      else inner(current-1, reduced.tail)
    }
    inner(i,l)
   }

   //Finds given element in list and returns its index, -1 otherwise
   def find(e:Int, l:List[Int]):Int = {
    @tailrec def inner(reduced: List[Int], current : Int): Int = {
      if(reduced.isEmpty) -1
      else if(reduced.head == e) current
      else inner(reduced.tail, current+1)
    }
    inner(l,0)
   }

   //Returns new list without element on ith position
   def delete(i:Int, l:List[Int]):List[Int] = {

    @tailrec def inner(current: Int, l1: List[Int], acc: List[Int]): List[Int] = {
      if(l1.isEmpty) acc
      else if(current == 0) inner(current -1, l1.tail, acc)
      else inner(current-1,l1.tail,l1.head :: acc)
    }
    inner(i,l,Nil).reverse
   }

   //Concat two lists into one: concat(List(1,2), List(3,4,5)) = List(1,2,3,4,5)
   def concat(l1:List[Int], l2:List[Int]):List[Int] = {
      @tailrec def inner(reduced_l1:List[Int],reduced_l2:List[Int],acc:List[Int]):List[Int] = {
        if(reduced_l1.isEmpty && reduced_l2.isEmpty) acc
        else if(!reduced_l1.isEmpty) inner(reduced_l1.tail,reduced_l2,reduced_l1.head::acc)
        else inner(Nil,reduced_l2.tail,reduced_l2.head::acc)
      }
      inner(l1,l2,Nil).reverse
   }

   //Merge two lists(sum elements on same poisitions)
   //If any of the lists its longer, ignore its tail
   //Example: zipperSum(List(1,2,3),List(1,2,3,4)) = List(2,4,6)
   def zipperSum(l1:List[Int], l2:List[Int]):List[Int] = {
    @tailrec def inner(reduced_l1:List[Int], reduced_l2:List[Int], acc:List[Int]): List[Int] = {
      if(reduced_l1.isEmpty || reduced_l2.isEmpty) acc
      else inner(reduced_l1.tail, reduced_l2.tail, (reduced_l1.head+reduced_l2.head)::acc)
    }
    inner(l1,l2,Nil).reverse
   }

   // Merge two lists in this manner: zipper(List(1,3,5,7), List(2,4,6)) = List(1,2,3,4,5,6)
   // If any of the lists its longer, ignore its tail
   def zipper(l1:List[Int], l2:List[Int]):List[Int] = {
    @tailrec def inner(reduced_l1:List[Int], reduced_l2:List[Int], acc:List[Int], choose: Int):List[Int] = {
      if(reduced_l1.isEmpty && reduced_l2.isEmpty) acc
      else {
        if(reduced_l1.isEmpty) inner(Nil,reduced_l2.tail, reduced_l2.head::acc,-1)
        else if(reduced_l2.isEmpty) inner(reduced_l1.tail,Nil, reduced_l1.head::acc,-1)
        else {
          if(choose == 0) inner(reduced_l1.tail, reduced_l2, reduced_l1.head::acc,1)
          else inner(reduced_l1, reduced_l2.tail, reduced_l2.head::acc,0)
        }
      }
    }
    inner(l1,l2,Nil,0).reverse
   }

   // Write a function that works as map
   // For example: map(List(1,2,3), x=>x+1 ) = List(2,3,4)
   def map(l: List[Int], f:Int=>Int):List[Int] = {
    @tailrec def inner(reduced_l1:List[Int],acc:List[Int]): List[Int] = {
      if(reduced_l1.isEmpty) acc
      else inner(reduced_l1.tail, f(reduced_l1.head)::acc)
    }
    inner(l,Nil).reverse
   }

   // Write a functions that works as foldLeft
   //
   def foldLeft(iVal:Int, l:List[Int], f:(Int,Int)=>Int):Int = {
    @tailrec def inner(reduced_l1: List[Int], acc: Int):Int = {
      if(reduced_l1.isEmpty) acc
      else inner(reduced_l1.tail, f(acc,reduced_l1.head))
    }
    inner(l,iVal) 
   }


  /*
   * Task 2. Write a function which will encode a string with so called LU/MTF encoding.
   */
  //Create starting list of ascii characters
  def createStartList:List[Char] = (0.toChar to 255.toChar).toList

  //In list l find char c, return its index and new list where c is moved to first place
  def findAndMTF(c:Char, l:List[Char]):(Int, List[Char]) = {
    val myList:List[Int] = l.map(x => x.toInt)

    val indeks = find(c.toInt,myList)
    val novSeznam = c::delete(indeks,myList).map(x => x.toChar)
    (indeks, novSeznam)
  }

  //Encode given string
  def encodeMTF(s:String):List[Int] = {
    val ascii = createStartList
    @tailrec def inner(reduced_s:String, acc:List[Int], current_ascii: List[Char]): List[Int] = {
      if(reduced_s.isEmpty) acc
      else {
        val tuple = findAndMTF(reduced_s.head, current_ascii)
        inner(reduced_s.tail, tuple._1::acc, tuple._2)
      }
    }
    inner(s,Nil,ascii).reverse
  }

  //Decode given encoded string
  def decodeMTF(l:List[Int]):String = {
    val ascii = createStartList
    @tailrec def inner(reduced_l1:List[Int], acc:String, current_ascii:List[Char]): String = {
      if(reduced_l1.isEmpty) acc
      else {
        val indeks = reduced_l1.head
        val intAscii = current_ascii.map(x=>x.toInt)
        val value = get(indeks,intAscii)
        inner(reduced_l1.tail, value.toChar+acc, value.toChar::delete(indeks,intAscii).map(x=>x.toChar))
      }
    }
    inner(l,"",ascii).reverse
  }


  /*
   * Task 3.
   * Improve functions from previous task so that in list for encoding there wont be only chars
   * but any pair of chars(pairs of two chairs).
   */
   //Create starting list of strings
   def createStartList2:List[String] = (0.toChar to 255.toChar).toList.map(x=>x+"")


   def findString(s: String, l:List[String]): Int = {
    @tailrec def inner(current: Int, reduced:List[String]): Int = {
      if(reduced.isEmpty) -1
      else if(reduced.head == s) current
      else inner(current+1, reduced.tail)
    }
    inner(0,l)
   }

   def deleteString(i: Int, l:List[String]):List[String] = {
    @tailrec def inner(current: Int, l1: List[String], acc: List[String]): List[String] = {
      if(l1.isEmpty) acc
      else if(current == 0) inner(current -1, l1.tail, acc)
      else inner(current-1,l1.tail,l1.head :: acc)
    }
    inner(i,l,Nil).reverse
   }

   //In list l find string s, return its index and new list where s is moved to first place
   def findAndMTF2(s:String, l:List[String]):(Int, List[String]) = {
    val indeks = findString(s,l)
    val novSeznam = s::deleteString(indeks,l)
    (indeks, novSeznam)
   }

   //Encode given string
   def encodeMTF2(s:String):List[Int] = {
    val ascii = createStartList2
    def inner(reduced_string: String, acc:List[Int], current_ascii:List[String]): List[Int] = {
      if(reduced_string.isEmpty) acc
      else {
        // Check first char
        val first_char = reduced_string.head+""

        val tail = reduced_string.tail
        // If we are at end, we cant check 2 chars
        if(tail.isEmpty) {
          val (current, new_ascii) = findAndMTF2(first_char, current_ascii)
          inner(tail, current::acc,new_ascii)
        }else{
          val second_char = tail.head+""
          val pair = first_char+second_char
          // We check 2 chars together
          val (current, new_ascii) = findAndMTF2(pair,current_ascii)
          // If pair is not on our list yet, we encode every char and put pair on our list
          if(current == -1){
            val (current1, new_ascii1) = findAndMTF2(first_char, current_ascii)
            val (current2, new_ascii2) = findAndMTF2(second_char, new_ascii1)
            inner(tail.tail,current2::current1::acc, pair::new_ascii2)
          } 
          // If pair is alredy on our list
          else {
            val (current3, new_ascii3) = findAndMTF2(pair, current_ascii)
            inner(tail.tail, current3::acc, new_ascii3)
          }
        }
      }
    }
    inner(s,Nil,ascii).reverse
   }



   // Decode given encoded string
   def decodeMTF2(l:List[Int]):String = {
    val ascii = createStartList2
    def inner(reduced_l1: List[Int], acc: String, current_ascii:List[String]): String = {
      if(reduced_l1.isEmpty) acc
      else {
        // Check first string
        val indeks = reduced_l1.head
        val valueFirst = getString(indeks,current_ascii)
        val left = reduced_l1.tail
        val new_ascii = valueFirst::deleteString(indeks,current_ascii)
        // If we are at end
        if(left.isEmpty) {
          inner(left, acc+valueFirst, current_ascii)
        } 
        // If string size is bigegr than 1, we alredy met him
        else if(valueFirst.length > 1) {
          inner(left, acc+valueFirst, valueFirst::deleteString(indeks,current_ascii))
        } 
        // Otherwise we check another char and merge them into pair and put on our list
        else {
          val indeks2 = left.head
          val valueSecond = getString(indeks2, new_ascii)
          val newString = valueFirst+valueSecond
          val newestAscii = valueSecond::deleteString(indeks2,new_ascii)
          val add = valueFirst + acc
          inner(left.tail,acc+newString, newString::newestAscii)
        }
        
      }
    }
    inner(l,"",ascii)
   }

}
