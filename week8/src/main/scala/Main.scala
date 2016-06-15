import scala.annotation.tailrec

//Naloga 1.
object InfLists {

  def from(n: Int): Stream[Int] = n #:: from(n+1)

  val exCoef:Stream[Double] = {
    def loop(n: Int):Stream[Double] = 1 #:: loop(n+1).map(x=>x/n)
    loop(1)
  }
  val cosCoef:Stream[Double] = {
    def loop(n:Int):Stream[Double] = 1 #:: loop(n+2).map(x=> -x / (n*(n-1)))
    loop(2)
  }
  val sinCoef:Stream[Double] = {
    def loop(n:Int):Stream[Double] = 1 #:: loop(n+2).map(x=> -x / (n*(n-1)))
    loop(3)
  }

  //val recaman:Stream[Int] = ???

  // n == st. elementov ki jih zelimo
  def toList[A](n: Int, s:Stream[A]):List[A] = {
    if (n > 0) s.head :: (toList((n-1), s.tail))
    else List()
  }
}

//Naloga 2.
object Collatz {
  def naturals:Stream[Int] = 1#::naturals.map(x=>x+1)
  def trace(n: Int): Stream[Int] = if (n==1) n#::Stream.empty else n #:: trace(if (n%2==0) n/2 else 3*n+1)
  def allTraces: Stream[Stream[Int]] = naturals.map(x=> trace(x))
  def findMaxSteps(n: Int): (Int, Int) = {    // se najbolje obnese (najkasneje vrže stackoverflow)
    @tailrec def findmax(in:Stream[Stream[Int]], out:Stream[Int]):(Int, Int) = {
      if (in.head.head == (n+1)) (out.head, out.length)
      else if (in.head.length > out.length) findmax(in.tail, in.head)
      else findmax(in.tail, out)
    }
    findmax(allTraces, 0#::Stream.empty)
  }
  def findMaxSteps2(n: Int): (Int, Int) = {
    val sizes = allTraces.take(n).map(_.length)
    val max_size = sizes.max
    val idx = sizes.indexOf(max_size)
    ((idx+1), max_size)
  }
  def findMaxSteps3(n: Int): (Int, Int) = {
    val stream = allTraces.toIterator.take(n).foldRight(0 #:: Stream.empty) { (a,b) => if (a.length > b.length) a else b}
    (stream.head, stream.length)
  }
  def findMaxSteps4(n: Int): (Int, Int) = {
    def stream:Stream[Int]  = allTraces.take(n).maxBy(_.length)
    (stream.head, stream.length)
  }
}

//Naloga 3.
object PowerSeries {
  //def apply(f: Int => Double): PowerSeries = ???
  val ex:PowerSeries = PowerSeries(InfLists.exCoef)
  val cos:PowerSeries = {
    def loop(n:Int):Stream[Double] = if (n%2 ==0) InfLists.cosCoef(n/2) #:: loop(n+1)else  0#:: loop(n+1)
    PowerSeries(loop(0))
  }
  val sin:PowerSeries = {
    def loop(n:Int):Stream[Double] = if (n%2 == 0) InfLists.sinCoef(n/2) #:: loop(n+1)else  0#:: loop(n+1)
    PowerSeries((0#::loop(0)))
  }
}

// coef == stream koeficientov a_i
case class PowerSeries(coef: Stream[Double]) {
  def apply(x: Double): Stream[Double] = {
    def xs:Stream[Double] = 1#::xs.map(y=>y*x)
    def stream:Stream[Double] = xs.zip(coef).map(p=>p._1*p._2)
    def loop(n:Int):Stream[Double] = stream.head #::loop(n+1).map(x=>x+stream(n))
    loop(1)
  }
  def +(other: PowerSeries): PowerSeries = PowerSeries(coef.zip(other.coef).map(p=>p._1 + p._2))
  // to je odvajanje polinomov
  def derive: PowerSeries = {
    def naturals:Stream[Int] = 1#::naturals.map(x=>x+1)
    PowerSeries(coef.drop(1).zip(naturals).map(p=>p._1*p._2))
  }
}

