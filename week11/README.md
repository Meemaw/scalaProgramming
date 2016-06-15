# Scalacheck

Napišite funkciji

```scala
def encode(s:String, move:Int)
def decode(s:String, move:Int)
```
prva naj zakodira podani niz (Cezarjev kod - zamik črk), druga pa ustrezno odkodira.

Zapišite lastnost(i) za ti dve funkciji.

Zapišite funkcijo:

```scala
def duplicate(l:List[Int]):List[Int]
```
ki vsak element seznama podvoji, npr:
```scala
duplicate(List(1,2,3)) == List(1,1,2,2,3,3)
```
Zapišite lastnosti za to funkcijo (preverite dolžine in če so vsebovani pravi elementi).

Napišite funkcijo:
```scala
def isPalindrome(s:String):Boolean
```
in zanjo zapišite dve lastnosti - najprej preverite pozitivne primere, nato pa še kakšnega negativnega.


Napišite svojo funkcijo za urejanje seznamov
```scala
def sort(l:List[Int]):List[Int]
```
in zanjo zapišite lastnost(i), ki morajo veljati za urejen seznam.



# Ponovitvene vaje


## Naloga 1.
Peanova števila so enostaven način za predstavitev naravnih števil.
Naravna števila lahko predstavimo kot število 0 (Z) in pa operacijo naslednik (angl. Successor - S).
V scali lahko torej števila predstavimo z naslednjo kombinacijo razredov in objektov:

```scala
trait Nat
case class S(n:Nat) extends Nat
case object Z extends Nat
```

Število 3 je torej predstavljeno kot
```scala
S(S(S(Z)))
```

Zapišite spremljevalni objekt Nat in apply(n:Int):Nat, ki pretvori celo število v Peanovo predstavitev.
Nato pa v traitu Nat zapišite metode za seštevanje, odštevanje, množenje in primerjavo (<).

```scala
def +(other:Nat):Nat
def *(other:Nat):Nat
def -(other:Nat):Nat
def <(other:Nat):Boolean
```


## Naloga 2.

Podane imate razrede za enostavna dvojiška drevesa (podatki nas ne zanimajo)

```scala
trait Tree
class Node(left: Tree, right:Tree) extends Tree
object Empty extends Tree
```

Zapišite funkcijo, ki bo generirala vsa možna binarna revesa velikosti n.
```scala
def genTrees(n:Int) : List[Tree]
```

za n = 3 dobimo npr. 5 dreves:
```scala

List(Node(Empty,Node(Empty,Node(Empty,Empty))),
 Node(Empty,Node(Node(Empty,Empty),Empty)),
 Node(Node(Empty,Empty),Node(Empty,Empty)),
 Node(Node(Empty,Node(Empty,Empty)),Empty),
Node(Node(Node(Empty,Empty),Empty),Empty))
```
Seveda so lahko v vašem seznamu tudi v drugačnem vrstnem redu.

## Naloga 3.

Najprej zapišite razred Complex
```scala
case class C(real: Double, img:Double)
```
In zapišite metode za seštevanje, odštevanje in množenje kompleksnih števil.

Predstavljajte si, da imate podano zaporedje kompleksnih števil:
```
z_{n+1} = z_n^2 + c
```
kjer je c neka kompleksna konstanta, z_0 pa naj bo 0+0i.

Za različne c dobimo različna zaporedja. Nekatera zaporedja divergirajo (pobegnejo v neskončnost) nekatera pa se večno vrtijo okrog 0.

Napišite funkcijo:

```scala
def cSeq(c:C):Stream[C]
```
ki za podani c vrne neskončni seznam zaporedja definiranega s c.


V splošnem je težko (nemogoče) dokazati ali neko zaporedje divergira, vendar obstajata dve enostavni pravili:
  1. Če je nek element v zaporedju oddaljen več kot 2 od 0, potem zaporedje divergira.
  2. Če do elementa z_1000 ne najdem nobenega elementa za katerega bi veljalo prvo pravilo, potem rečemo, da zaporedje zelo verjetno ne pobegne.

Zapišite funkcijo
```scala
def isDivergent(seq:Stream[C]):Boolean
```
ki vrne true, če zaporedje divergira in false sicer.

Na koncu za vsa kompleksna števila v kvadratu (-2-i) do (1+i) in resolucijo 60x20 (po x se premikajte po koraku 0.05, po y osi pa s korakom 0.1) izpišite zvezdico če zaporedje divergira, in presledek v nasprotnem primeru.

Seznam vseh koordinat lahko npr. naredite tako:
```scala
val mand = for(y <- -1.0 to 1.0 by 0.1; x <- -2.0 until 1.0 by 0.05) yield (y,x)
```

Kot rezultat lahko pričakujete nekaj takega:
```
************************************************************
************************************************************
************************************    ********************
************************************    ********************
****************************** **          *****************
******************************                 *************
****************************                   *************
***************************                     ************
*****************       **                      ************
****************         *                      ************
***** ******                                  **************
****************         *                      ************
*****************       **                      ************
***************************                     ************
****************************                   *************
******************************                 *************
****************************** **          *****************
************************************    ********************
************************************    ********************
************************************************************
************************************************************
```
