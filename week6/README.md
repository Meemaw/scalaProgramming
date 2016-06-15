# Naloge v šestem tednu

V šestem tednu bomo spoznali podatkovne strukture v funkcijskih programskih jezikih.
Osredotočili se bomo predvsem na nekaj prijemov, ki bistveno pohitrijo dve podatkovni strukturi, obenem pa ohranijo učinkovito (tako prostorsko kot časovno) persistentnost teh podatkovnih struktur.

## Naloga 1.

Implementirajte podatkovno strukturo vrsta na dva načina. Najprej implementirajte naiven način dela z vrsto (kot linearni seznam), nato pa še izboljšano obliko z dvema seznamoma - prvi naj predstavlja prednji del vrste, drugi pa zadnji del vrste.

NB. Vrste hranijo sezname celih števil.
```scala
trait Q1
case class QC1(el: List[Int], next: Q1) extends Q1
case object QEmpty1 extends Q1
```

```scala
trait Q2
case class QC2(front: List[List[Int]], end: List[List[Int]]) extends Q2
case object QEmpty2 extends Q2
```

V obeh traitih implementirajte metode:
```scala
def enQ(el: List[Int]): Q1 [Q2]
def deQ:(List[Int], Q1 [Q2])
```
Prva metoda naj vrne novo vrsto, kjer je en element dodan na konec te vrste. Druga metoda pa naj vrne element iz začetka vrste ter novo vrsto, kjer tega elementa ni več na začetku. Pri drugi vrsti seveda uporabite trik, ki smo si ga ogledali na predavanjih.

Nato napišite funcijo (eno v spremljevalnem objektu Q1, drugo v spremljevalnem objektu Q2), ki bo za podano zaporedje operacij vrnila število dostopov do elementov.
```scala
def countAcc(seq: List[Option[List[Int]]]): Int
```
Primer zaporedja zahtevkov za vrsto:
```scala
val seq = List(Some(List(1)), Some(List(2)), None, None )
```
To zaporedje predstavlja najprej dve vstavljanji v vrsto, nato pa dve odvzemanji iz vrste.

## Naloga 2. :crown:

Zgornjo vrsto (Q2) uporabite za implementacijo iskanja poti v labirintih.
Labirint naj bo podan kot niz, kjer bo znak * predstavljal zid, presledek pa prosti prehod. Vrstice labirinta so ločene z znakom za novo vrstico. Vrstice naj bodo oštevilčene od 0 do n, stolpci pa od 0 do m. Vhod v labirint naj bo vedno na poziciji (1,0), t.j. vrstica 1 in stolpec 0, izhod iz labirinta pa na poziciji (n-1, m).

```
****************
            *  *
****  ****  *  *
*     *        *
*  *************
*     *        *
****  *  ****  *
*     *  *     *
*  *******  ****
*
****************
```

Vaša naloga bo, da poiščete pot v tem labirintu od vhoda do izhoda, z iskanjem v širino.

Implementirajte razred
```scala
class Maze(nRows:Int, nCols:Int, cells:List[Boolean])
```
V seznamu `cells` je shranjeno stanje labirinta - če je vrednost na neki poziciji `true`, potem naj bo to prehodna celica, če pa je `false` pa naj bo to neprehodna celica. Dvodimenzionalno koordinato celice (r,c) enostavno preslikajte v enodimenzionalno, da dobite njeno pozicijo v seznamu `cells`.

Zapišite tudi spremljevalni objekt `Maze` v katerem napišite funkcijo
```scala
def apply(m:String):Maze
```
ki iz podanega niza ustvari ustrezen Maze objekt.


Napišite funkcijo, ki izračuna rešitev tega labirinta in jo vrne kot seznam pozicij v seznamu `cells`.
```scala
def solution:List[Int]
```

Ta funkcija naj preišče labirint tako, da začne na celici (1,0) in vsakič doda v vrsto sosednje celice, ki so prehodne in se na trenutni poti še niso pojavile. Za vsako celico morate torej hraniti tudi pot, po kateri ste do te celice prišli.

Zato ste v prejšnji nalogi implementirali vrsto, ki hrani sezname celih števil. V vrsto boste torej postavljali poti v labirintu, po katerih ste prišli do trenutno aktualnih celic. Vrsta pa bo poskrbela, da boste labirint preiskovali v širino.

Nazadnje implementirajte še funkcijo

```scala
def solutionView:String
```
ki vrne niz s predstavljeno rešitvijo. Primer take rešitve si lahko ogledate spodaj.

```
****************
ooooo       *  *
****o ****  *  *
* ooo *        *
* o*************
* ooo *        *
****o *  ****  *
* ooo *  *     *
* o*******  ****
* oooooooooooooo
****************
```
