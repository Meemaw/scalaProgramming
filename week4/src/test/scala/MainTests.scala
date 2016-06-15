import org.specs2.mutable._

class Week4Tests extends Specification {


	"Properly create BTS" should {

		val t1 = EmptyT
		val t2 = new Node(5,EmptyT,EmptyT)
		val t3 = new Node(2, new Node(5,new Node(2,EmptyT,EmptyT),EmptyT), new Node(3,EmptyT,new Node(6,EmptyT,EmptyT)))
		val t4 = BST(1,4,5)
		val t44 = new Node(1, new Node(5,EmptyT,EmptyT),EmptyT)
		val t44copy = new Node(1, new Node(5,EmptyT,EmptyT),EmptyT)
		val t3copy = new Node(2, new Node(5,new Node(2,EmptyT,EmptyT),EmptyT), new Node(3,EmptyT,new Node(6,EmptyT,EmptyT)))
		val t5 = BST(9,4,5,8,10)
		val d:BST = BST(4,7,2,8,5,3,9)

		"isEmpty function" in {
			t1.isEmpty mustEqual true
			t2.isEmpty mustEqual false
			t4.isEmpty mustEqual false
		}
		"head function" in {
			t2.head mustEqual 5 
			t4.head mustEqual 1
		}
		"& function" in {
			(t1 & 5).toList mustEqual List(5)
		}
		"size function" in {
			t1.size mustEqual 0
			t2.size mustEqual 1
			t3.size mustEqual 5
			t4.size mustEqual 3
		}
		"toList function" in {
			t5.toList mustEqual List(9,4,5,8,10)
			t4.toList mustEqual List(1,4,5)
			t1.toList mustEqual Nil
			t2.toList mustEqual List(5)
			t3.toList mustEqual List(2,5,2,3,6)
			d.toList mustEqual List(4, 2, 3, 7, 5, 8, 9)
		}
		"toSortedList function" in {
			t5.toSortedList mustEqual List(4,5,8,9,10)
			t1.toSortedList mustEqual Nil
			d.toSortedList mustEqual List(2,3,4,5,7,8,9)
		}
		"apply function" in {
			t2(0) mustEqual 5
			t3(0) mustEqual 2
			t5(2) mustEqual 8
			d(0) mustEqual 2
			d(1) mustEqual 3
			d(6) mustEqual 9
		}
		"map function" in {
			def f1:Int=>Int = x => x+1
			def f2:Int=>Int = x => if(x % 2 == 0) x+100 else x
			t2.map(f1).toList mustEqual List(6)
			t5.map(f1).toList mustEqual List(10,5,6,9,11)
			d.map(f1).toList mustEqual List(5,3,4,8,6,9,10)
			d.map(f2).toList mustEqual List(104, 102, 3, 7, 5, 9, 108)
		}
		"merge function" in {
			t1.merge(t2).toList mustEqual List(5)
			t4.merge(t5).toList mustEqual List(1, 4, 4, 5, 5, 9, 8, 10)
			d.merge(t5).toList mustEqual List(4, 2, 3, 4, 7, 5, 5, 8, 8, 9, 9, 10)
		}
		"filter function" in {
			d.filter(x => x==4).toList mustEqual List(4)
			BST(5,2,4,2).filter(x=> x== 4 || x== 2).toList mustEqual List(2,2,4) 
		}
		"foldInOrder" in {
			d.foldInOrder(0)((acc,x) => acc+1) mustEqual d.size
			d.merge(t5).foldInOrder(0)((acc,x) => acc+x) mustEqual 74
		}
		"equals" in {
			t2.equals(t2) mustEqual true
			d.equals(t2) mustEqual false
			d.equals(d) mustEqual true
			t44.equals(t44copy) mustEqual true
			t3.equals(t3copy) mustEqual true
			t3.equals(t4) mustEqual false
			t1.equals(t1) mustEqual true
		}

		"get cords" in {
			val t1 = EmptyT
			t1.getCoords(0,0) mustEqual List()
			BST(1).getCoords(0,0) mustEqual List((0,0,1))
			BST(1,2).getCoords(0,0) mustEqual List((0,0,1),(2,1,2))
			BST(4,2,5,6).getCoords(0,0) mustEqual List((2,0,4),(0,1,2),(4,1,5),(6,2,6))
			BST(5,4,3).getCoords(0,0) mustEqual List((4,0,5), (2,1,4), (0,2,3))
		}

		"toString" in {

			BST(5,2,8,5,3,6,2).toString mustEqual 
"""
........5
..2.........8
2.....5...6
....3
"""
			BST(1,2,3,4,5).toString mustEqual
"""
1
..2
....3
......4
........5
"""

		BST(3,4,5,1,1,1,5).toString mustEqual
"""
......3
....1...4
..1.........5
1.........5
"""

		BST(5,4,3,2,1,6,7,8,9,5,4,3,2,6,7,8,8,9,10).toString mustEqual
"""
................5
............4.......6
........3.....5...6.....7
....2.....4...........7.......8
1.....3.....................8.....9
..2.......................8.....9...10
"""	
		BST().toString mustEqual "  EmptyT  "
		}
	}

  //testirajte različno ustvarjanje BST-ja

  //testirajte pridobivanje osnovnih informacij

  //testirajte BST kot urejeno tabelo

  //testirajte operaciji nad dvemi drevesi

  //testirajte višjenivojske funkcije za BST

  //testirajte lep izris drevesa

}
