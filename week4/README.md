# Tasks in week four



## Task 1.
Write classes which defines Binary Search Tree(BST).
All operations must leave tree untouched, tree must be **immutable**. 


### Tree creating
```scala
val a:BST = EmptyT  //Empty tree
val b:BST = new Node(3, new Node(2,EmptyT,EmptyT), EmptyT)
 ```

Adding of new element
```scala
val c = a & 5 // return Node(5, EmptyT, EmptyT)
```

```scala
val d:BST = BST(4,7,2,8,5,3,9)
```

### Informations about tree

```scala
b.isEmpty //return false
b.head // return 3
b.left // return Node(2,EmptyT,EmptyT)
b.size // return 2
```
### BST as ordered array
Implement folowing functions
```scala
d(0)  //return smallet element in tree: 2
d(1)  //Second smallest element: 3
d(6)  //Seventh smallest element: 9
d.toSortedList //return ordered list of elements: List(2,3,4,5,7,8,9)
```

### Operations on two trees
```scala
def merge(other:BST):BST
override def equals(other:BST):Boolean
```
### Higher order functions

```scala
def map(f:Int=>Int):BST
def foldInOrder(startVal:Int)(f:(Int, Int)=>Int):Int
def filter(f:Int=>Boolean):BST
```

## Task 2. :crown:
Write a function that will return string with nicely drawn binary tree:
```scala
override def toString:String
```

 ```scala
 def getCoords(depth:Int, offset:Int):List[(Int,Int,Int)]
 ```

 Example:
 ```scala
 val a = BST(5,2,8,5,3,6,2)
 ```
We get list of coordinates:
```scala
List((1,2,2), (3,1,2), (4,3,3), (5,2,5), (7,0,5), (9,2,6), (11,1,8))
```
And example of drawing:
```
......5
..2.......8
2...5...6
...3
```
Could be better. :smile:.
