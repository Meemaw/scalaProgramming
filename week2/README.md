# Tasks in second week



## Task 1.
Functions to present Set.
```scala
type Set = Int => Boolean
```
```scala
def singleton(n:Int): Set
def interval(a:Int, b:Int): Set
```

Union and intersect of Sets
```scala
def union(s1:Set, s2:Set): Set
def intersect(s1:Set, s2:Set): Set
```
To test implementation of previous functions
```scala
def dedup(l: List[Int]): List[Int]
```
Remove all duplicates from given List

Some basic functions to work with lists.
```scala
val l = List(1,2,3,4,5) //constructing new list
l1.isEmpty //check if list is empty
l1.head //first element of a list
l1.tail //list without first element
0::l1 //adding element to head of list

```


## Task 2.
Real variable functions

```scala
type RealFunc = Double => Double
```

```scala

def sum(f1:RealFunc, f2:RealFunc):RealFunc

def mult(f1:RealFunc, f2:RealFunc):RealFunc

def compose(f1:RealFunc, f2:RealFunc):RealFunc

def derive(f:RealFunc):RealFunc
```


## Task 3.:crown:
Every function `Char=>Char` can be interpreted as encoding function

```scala
type Coder = Char => Char
```

Write some functions to work with encoding.
 ```scala
 def encode(s:String, f:Coder):String
 ```
```scala
def isBijective(f:Coder):Boolean
```

```scala
def inverse(f:Coder):Coder
```

```scala
def decode(s:String, f:Coder):String
```
