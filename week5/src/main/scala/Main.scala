
import scala.collection.immutable.HashMap


trait Expr{
	//implementirajte funkcijo value, ki vrne vrednost tega izraza npr. Some(3.5), če
	// pa za tega izraza ni mogoce ovrednotiti pa naj vrne None.
	// Funkcija ima opcijski parameter, v katerem so podane vrednosti spremenljivk
	// kot HashMap - ime spremenljivke preslika v neko vrednost.
	def value(h:HashMap[String, Double]= HashMap()):Option[Double] = {

		def eval(current:Expr):Option[Double] = current match {
			case Num(x) => Some(x)
			case Var(x) => if(h.contains(x)) Some(h(x)) else None
			case Sum(prvi,drugi) => for{x <- eval(prvi); y <- eval(drugi)} yield x + y
			case Prod(prvi,drugi) => for{x <- eval(prvi); y <- eval(drugi)} yield x * y
			case Quot(prvi, drugi) => for{x <- eval(prvi); y <- eval(drugi)}  yield x / y
			case Pow(prvi, drugi) => for{x <- eval(prvi)} yield scala.math.pow(x, drugi)
		}

		eval(this)
	}

	//v tem traitu implementirajte funkciji simplify in derive
	def derive(v:Var):Expr = {

		def deriv(current:Expr):Expr = current match {
			case Num(x) => Num(0.0)
			case Var(x) => if(x == v.x) Num(1.0) else Num(0.0)
			case Sum(prvi, drugi) => Sum(deriv(prvi), deriv(drugi))
			case Prod(prvi, drugi) => Sum(Prod(deriv(prvi), drugi), Prod(prvi, deriv(drugi)))
			case Quot(prvi, drugi) => Quot( Sum(Prod(deriv(prvi), drugi), Prod(Num(-1.0), Prod(prvi, deriv(drugi)))), Pow(drugi,2) )
			case Pow(Num(prvi), potenca) => Num(0.0)
			case Pow(prvi, potenca) => Prod(Num(potenca), Pow(prvi, potenca-1))
		}

		deriv(this)
	}

	//carska naloga
	def simplify:Expr = {

		def simplifyChildren(current: Expr): Expr = current match {
			case Sum(x, y) => Sum(simple(x), simple(y))
			case Prod(x, y) => Prod(simple(x), simple(y))
			case Quot(x, y) => Quot(simple(x), simple(y))
			case Pow(x, y) => Pow(simple(x), y)
			case x => x
		}

		def simple(current:Expr):Expr = simplifyChildren(current) match {
			case Sum(Num(0), Num(0)) => Num(0)
			case Sum(Num(0), x) => x
			case Sum(x, Num(0)) => x
			case Sum(x,y) => Sum(simple(x), simple(y))

			case Prod(Num(0), x) => Num(0)
			case Prod(x, Num(0)) => Num(0)
			case Prod(Num(1), x) => x
			case Prod(x, Num(1)) => x
			case Prod(x,y) => Prod(simple(x), simple(y))

			case Quot(Num(0), x) => Num(0)
			case Quot(x, Num(1)) => x
			case Quot(x,y) => Quot(simple(x), simple(y))

			case Pow(x, 0) => Num(1)
			case Pow(x, 1) => x
			case _ => current
		}
		simple(this)
	}
}

object Expr{
	//v tem objektu implementirajte funkijo apply, ki iz niza, ki predstavlja izraz v postfiksni obliki zgradi Expr.

	def apply(s: String):Expr = {
		if(s.isEmpty) throw new Exception
		else {
			// splita string po presledkih
			val list = s.split(" ").toList
			// funkcija preveri ce je string stevilo
			def isAllDigits(x: String) = {
				if(x forall Character.isDigit) true
				else if(x.startsWith("-")) true
				else false
			}

			// gre skozi list in generira Expr
			def skoziList(listLeft:List[String], stack:List[Expr]): Expr = {
				if(listLeft.isEmpty) stack.head
				else {
					val check = listLeft.head
					check match {
						case x if isAllDigits(x) => skoziList(listLeft.tail, Num(x.toInt)::stack)
						case "*" => skoziList(listLeft.tail, Prod(stack.tail.head, stack.head)::stack.tail.tail)
						case "/" => skoziList(listLeft.tail, Quot(stack.tail.head, stack.head)::stack.tail.tail)
						case "+" => skoziList(listLeft.tail, Sum(stack.tail.head, stack.head)::stack.tail.tail)
						case x if x.startsWith("^") => skoziList(listLeft.tail, Pow(stack.head, x.tail.toInt)::stack.tail)
						case _ => skoziList(listLeft.tail, Var(check)::stack)
					}
				}
			}
			skoziList(list,List())
		}

	}
}



//zapišite implementacije case razredov:
case class Var(x:String) extends Expr
case class Num(x:Double) extends Expr
case class Sum(e1:Expr, e2:Expr) extends Expr
case class Prod(e1:Expr, e2:Expr) extends Expr
case class Quot(e1:Expr, e2:Expr) extends Expr
case class Pow(e:Expr, k:Int) extends Expr
//Var
//Num
//Sum
//Prod
//Quot
//Pow
