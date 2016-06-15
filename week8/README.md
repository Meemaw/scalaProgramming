# Naloge v osmem tednu

V osmem tednu se bomo lotili lenega izračuna in lenih seznamov (Stream) v Scali.

## Naloga 1.

Zapišite nekaj funkcij in neskončnih seznamov (nekatere boste potrebovali pri carski nalogi).

Funkcija
```scala
def from(n:Int):Stream[Int]
```
naj vrne neskončnen seznam celih števil, ki se začnejo pri n.

Zapišite tri neskončne sezname, ki predstavljajo koeficiente v potenčni (Taylorjevi) vrsti funkcij e^x, sin in cos.
```scala
val exCoef:Stream[Double]
val sinCoef:Stream[Double]
val cosCoef:Stream[Double]
```
Za pomoč si oglejte te koeficiente na:
http://people.math.sc.edu/girardi/m142/handouts/10sTaylorPolySeries.pdf

Kot enovrstičnico zapišite Recamanovo zaporedje:
http://mathworld.wolfram.com/RecamansSequence.html

```scala
val recaman:Stream[Int]
```
Preučite kako smo zapisali Fibonaccijevo zaporedje in poskusite spisati nekaj podobnega za to zaporedje.

## Naloga 2.

Problem 3n+1 (znan kot Collatzova domneva) je eden klasičnih problemov v računalništvu.
Denimo, da imamo podan tak program (napisan zelo nefunkcijsko):

```scala
def f(n:Int)={
  if(n==1)return
  if(n%2==0)
    f(n/2)
  else
    f(3*n+1)
}
```
Vprašanje je, ali se podani program ustavi za vsako število n? Collatzova domneva je, da se res ustavi, vendar tega še nobenemu ni uspelo dokazati.
Več o definiciji problema in nekaj primeri lahko najdete na:
http://sl.wikipedia.org/wiki/Collatzova_domneva

Vaša naloga je, da v objektu Collatz definirate dve funkciji - lahko še kakšno pomožno :).
Prva funkcija naj bo
```scala
def trace(n:Int):Stream[Int]
```
ki vrne sled izvajanja funkcije 3n+1 (funkcije f zgoraj), npr. sled za n=7 je sled.
```scala
Stream(7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1)
```
Seveda ne boste dobili celotnega seznama naenkrat, ampak tipično nekaj takega
```scala
Stream(7,?)
```

Druga funkcija naj vrne Stream vseh možnih sledi (sledi za vsa naravna števila):
```scala
def allTraces:Stream[Stream[Int]]
```

seznam naj bo seveda neskončen. Uporabite ta seznam v funkciji

```scala
def findMaxSteps(n:Int):(Int, Int)
```
Ta funkcija naj vrne število med 1 in n, pri katerem je sled najdaljša, obenem pa še dolžino te sledi (zato ta funkcija vrača par).



## Naloga 3.:crown:

Potenčne vrste predstavljajo posplošitev polinomov - lahko jih gledamo tudi kot na polinome z neskončno koeficienti.  

Veliko funkcij lahko izrazimo s potenčnimi vrstami oblike:
![](img/series.png)

Napišite razred:
```scala
class PowerSeries(coef: Stream[Double])
```
s katerimi predstavite take funkcije - coef pa je neskončen seznam koeficientov a_i.

V tem razredu implementirajte funkcijo:
```scala
def apply(x:Double):Stream[Double]
```
ki vrne neskončen seznam vedno boljših približkov funkcije predstavljene s to potenčno vrsto v točki x. Recimo za funkcijo e^x v točki 1 bo prvih 10 elementov tega seznama enakih:
```
Stream(1.0, 2.0, 2.5, 2.6666666666666665, 2.708333333333333, 2.7166666666666663, 2.7180555555555554, 2.7182539682539684, 2.71827876984127, 2.7182815255731922, 2.7182818011463845, ?)

```

Nato implementirajte seštevanje dveh potenčnih vrst
```scala
def +(other:PowerSeries):PowerSeries
```
in odvajanje potenčne vrste
```scala
def derive:PowerSeries
```

Implementirajte tudi spremljevalni objekt.

```scala
object PowerSeries{
  def apply(coef:Int=>Double):PowerSeries
  val ex:PowerSeries = ???
  val cos:PowerSeries = ???
  val sin:PowerSeries = ???
}
```
Funkcija apply omogoča ustvarjanje potenčne vrste s pomočjo podane funkcije, ki za podani indeks izračuna koeficient a_i.
Ustvarite še primere potenčnih vrst: za funkcijo e^x, cos(x) in sin(x).
