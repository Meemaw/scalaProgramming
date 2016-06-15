# Tasks in week three




## Task 1. (Lists)
Implementation of some basic functions to work with lists.

**From standard library  only `head`, `tail` in `isEmpty` are used, and ofcourse counstructors `Nil`, `List()` and `::`.**

Basic operations on lists:
```scala
def get(i:Int, l:List[Int]):Int

def find(e:Int, l:List[Int]):Int

def delete(i:Int, l:List[Int]):List[Int]
```

Merging of lists:

```scala
def concat(l1:List[Int], l2:List[Int]):List[Int]

def zipperSum(l1:List[Int], l2:List[Int]):List[Int]

def zipper(l1:List[Int], l2:List[Int]):List[Int]
```

Transformations on lists:

```scala
def map(l: List[Int], f:Int=>Int):List[Int]

def foldLeft(iVal:Int, l:List[Int], f:(Int,Int)=>Int):Int
```


## Task 2. (MTF encoding)
Task 2. Write a function which will encode a string with so called LU/MTF encoding.

```scala
def createStartList:List[Char]
```

```scala
def findAndMTF(c:Char, l:List[Char]):(Int, List[Char])
```


With help of upper two functions implement encode and decode of a string:

```scala
def encodeMTF(s:String):List[Int]

def decodeMTF(l:List[Int]):String
```


## Task 3. (MTF encdoing ++) :crown:
Improve functions from previous task so that in list for encoding there wont be only chars but any pair of chars(pairs of two chairs).
```scala
def createStartList2:List[String] 
def findAndMTF2(s:String, l:List[String]):(Int, List[String])
def encodeMTF2(s:String):List[Int]
def decodeMTF2(l:List[Int]):String
```
