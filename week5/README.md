# Naloge v petem tednu


V petem tednu predmeta Scala, se bomo ukvarjali z ujemanjem vzorcev in case razredi. Ujemanje vzorcev bomo spoznali skozi delo z aritmetičnimi izrazi (ki jih lahko vidimo tudi kot funkcije ene ali več spremenljivk).


## Naloga 1.
Implementirajte razred (trait) `Expr`, ki bo predstavljal aritmetični izraz z operacijami seštevanje, množenje, deljenje in potenciranje. V izrazu pa lahko nastopajo spremenljivke in realna števila. Spremenljivke so lahko poljuben niz črk in številk, ne smejo pa se začeti s številko.

Izraz
`a+15+b*3`
bi predstavili v Scali na sledeči način:

```scala
Sum(Var("a"),Sum(Num(15.0),Prod(Var("b"),Num(3.0))))
```

Spremenljivka naj bo torej predstavljena z objektom tipa `Var`, celo število z objektom tipa `Num`, dovoljene operacije v teh izrazih pa naj bodo:
  - vsota - `Sum(e1:Expr, e2:Expr)`
  - produkt - `Prod(e1:Expr, e2:Expr)`
  - kvocient - `Quot(e1:Expr, e2:Expr)`
  - potenca - `Pow(e:Exp, k:Int)`

Napišite funkcijo `apply`, ki na podlagi izraza podanega v postfiksni obliki zgradi `Expr`. Implementirajte jo v spremljevalnem objektu `Expr`.
```scala
def apply(s:String):Expr
```
Ta funkcija sprejme niz, ki predstavljajo izraz v postfiksni obliki (posamezni elementi so ločeni z enim presledkom). Vrniti mora objekt tipa `Expr`, ki predstavlja podani izraz. Npr.
```scala
Expr("a 5 + 4 / 2 c ^5 + *")
```
ustvari izraz  `(a+5)/4*(2+c^5)` - oz. v Scali
```scala
Prod(Quot(Sum(Var("a"),Num(5.0)),Num(5.0)),Sum(Num(2),Pow(Var("c"),5)))
```
Predpostavite lahko, da je podani vhod v pravilni obliki, torej ne potrebujete posebne kontrole za napačne definicije izrazov.

## Naloga 2.

V traitu `Expr` definirajte funkcijo `value`:
```scala
def value(h:HashMap[String, Double] = HashMap()):Option[Double]
```
Ta funkcija naj vrne izračunano vrednost podanega izraza e. V HashMap-u so podane vrednosti spremenljivk, ki se pri izračunu uporabljajo. Ta hash tabela je opcijski parameter. Ker so lahko v izrazu še vedno nedefinirane spemenljivke (spremenljivke brez vrednosti v hashmap-u), lahko ta funkcija vrne vrednost `None`, sicer pa `Some(v)`, kjer je `v` izračunana vrednost.
Primeri uporabe funkcije
```scala
Sum(Num(5.0),Num(8.0)).value()
```
vrne `Some(13.0)`, če pa uporabimo še HashMap:
```scala
Sum(Num(5),Var("a")).value(HashMap("a"->3))
```
vrne `Some(8.0)`.

Pri implementaciji te funkcije se boste srečali z dvema strukturama, ki ju še ne poznate.

  1. HashMap -> to je nespremenljiva hash tabela, ki jo inializiramo npr.
  ```scala
  val h = HashMap("a"->17.0, "b"->22.0, "x"->10032.041)
  ```
  vrednost za neki niz pa dobimo kar kot `h("x")`. Za neki niz preverimo, če je že v tabeli z `h.contains("a")`. Za uporabo te strukture morate uvoziti `import scala.collection.immutable.HashMap`.
  2. Option[Double] -> to je tip, ki definira rezultat, ki je lahko veljavno število ali pa nedefinirano. V prvem primeru imamo na voljo konstruktor `Some(stevilo)`, v Drugem pa `None`. Da izveste, če je  neka vrednost definirana (torej, če ni None) lahko uporabite `a.isDefined`, za pridobiti vrednost iz `Some(x)`, pa uporabite metodo `get`. Primer:
  ```scala
  val opt = Some(1.2)
  if(opt.isDefined) opt.get else -1
  ```

## Naloga 3.
V traitu `Expr` definirajte funkcijo `derive`:
```scala
 def derive(v:Var):Expr
```
Ta funkcija sprejme spremenljivko in vrne odvod izraza po tej spremenljivki. Za osvežitev spomina, si lahko ogledate pravila odvajanja na [tej povezavi](http://www.mathsisfun.com/calculus/derivatives-rules.html).
## Naloga 4.:crown:
V traitu `Expr` definirajte funkcijo `simplify`:
```scala
 def simplify(e:Expr):Expr
```
ki podani izraz poskusi čimbolj poenostaviti. Npr. izraz `a+2*3` naj se poenostavi v `a+6`, ali pa izraz `0+a` naj se poenostavi v `a`, itd.
Implementirajte vsaj 8 pravil za poenostavitev izrazov, v kodi jasno označite kje in kakšne vrste poenostavitev ste implementirali.
