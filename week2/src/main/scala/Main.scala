import scala.annotation.tailrec



class Main {
  /* Task 1. Sets
 * Functions of type Int => Boolean could also be seen as presentation of Sets.
   At this task, you should write some functions, which enables work with presentation of sets.
 */

  type Set = Int => Boolean

  // Write a function which for given number returns Set with only this number
  def singleton(n: Int): Set = Set(n)

  def add(s1: Set, n: Int): Set = union(s1, singleton(n))

  //  Write a function which returns union of two Sets.
  def union(s1: Set, s2: Set): Set = i => s1(i) || s2(i)

  //  Write a function which returns intersect of two Sets.
  def intersect(s1: Set, s2: Set): Set = i => s1(i) && s2(i)

  //  Write a function which returns set on internal from a to b.
  def interval(a: Int, b: Int): Set = x => x <= b & x >= a
  


  // Write a function that removes all duplicates from Set.
  def dedup(l: List[Int]): List[Int] = {


    @tailrec def uniq(s1: Set, l1: List[Int], acc: List[Int]): List[Int] = {
      if(l1.length == 0) acc
      // ce je element ze na setu gremo naprej
      else if(s1(l1.head)) uniq(s1, l1.tail, acc)
      // ce se elementa ni na setu, ga dodamo na set in na list
      else uniq(add(s1,l1.head), l1.tail, l1.head :: acc)
    }
    uniq(Set(), l, List[Int]()).reverse
  }




  /* Task 2. Real variable functions
  */
  type RealFunc = Double => Double

  val eps = 0.00001

  

  //Compositum of two functions
  def compose(f1: RealFunc, f2: RealFunc): RealFunc = i => f2(f1(i))

  //Sum of two functions
  def sum(f1: RealFunc, f2: RealFunc): RealFunc = i => f1(i) + f2(i)

  //Produkt of two functions
  def mult(f1: RealFunc, f2: RealFunc): RealFunc = i => f1(i) * f2(i)

  //Derivative of a function
  def derive(f: RealFunc): RealFunc = x => ((f(x+eps) - f(x)) / eps)

  /* Task 3. Encoding
  *  Every function of type Char=>Char can be taken as encdoing function. Write some functions to work with such encoding functions.
  */
  type Coder = Char => Char
  val myWorld = ('a' to 'z')
  //Encode given string with function
  def encode(s: String, f: Coder): String = s.map(f)

  //Return if function is bijective
  def isBijective(f: Coder): Boolean = {
    val chars: List[Char] = myWorld.toList
    chars.length == chars.map(f).distinct.length
  }

  //Return inverse of a function
  def inverse(f: Coder): Option[Coder] = {
  	//If function is not bijective, it has no inverse
    if(!isBijective(f)) None
    else {
    	val pairs = myWorld.toList.map(x => (f(x), x))
    	def myCoder: Char => Char = x => pairs.filter(novi => x == novi._1).head._2
      Some(myCoder)
    }
  }

  //Decode given string with inverse of what it was encoded with
  def decode(s: String, f: Coder): String = s.map(inverse(f).get)

}
