import scala.io._
import scala.swing._
import java.io._
import java.lang.NumberFormatException


// Glavni objekt z vsemi impliciti in kljucnimi besedami
object CSVLang {
  // ----------------- Kljucne besede -------------------------------
  trait ActionKeyword
  case object drop extends ActionKeyword
  case object swap extends ActionKeyword
  case object revrow extends ActionKeyword

  trait DataKeyword
  case object ask extends DataKeyword
  case object out extends DataKeyword
  case object screen extends DataKeyword

  trait FormatKeyword
  case object int extends FormatKeyword
  case object dbl extends FormatKeyword
  case object ign extends FormatKeyword

  trait ExpressionKeyword
  case object acc extends ExpressionKeyword
  case object col extends ExpressionKeyword

  // ----------------- Impliciti za CsvData -------------------------------

def getData(filename: String): CsvData = {
  val source = scala.io.Source.fromFile(filename)
  val lines = try source.mkString finally source.close()
  val vrstice: Stream[String] = lines.split("\n").toStream
  val stream: Stream[Stream[String]] = vrstice.map(x => x.split(",").toStream)
  CsvData(stream, ',')
}

implicit def toCsvData(data: String): CsvData = {
  val str = data takeRight 3
  if(str == "csv") getData(data)
  else {
      val vrstice: Stream[String] = data.split("\n").toStream
      val stream: Stream[Stream[String]] = vrstice.map(x => x.split(",").toStream)
      CsvData(stream, ',')
  }
}

implicit def toCsvData(file: String, sep: Char): CsvData = {
    val source = scala.io.Source.fromFile(file)
    val lines = try source.mkString finally source.close()
    val vrstice: Stream[String] = lines.split("\n").toStream
    val stream: Stream[Stream[String]] = vrstice.map(x => x.split(sep).toStream)
    CsvData(stream, sep)
}

  // --------------------- Impliciti za CsvAction ----------------------

  implicit def parseAction(x :(ActionKeyword, Int)): CsvAction = {
    x._1 match { case `drop` => DropCol(x._2) }
    
  }

  implicit def parseAction(x:DataKeyword): CsvAction = {
    x match { case `screen` => ScreenOut(true) }
  }


  implicit def parseAction(x :(ActionKeyword, Int,Int)): CsvAction = {
    x._1 match { case `swap` => SwapCol(x._2, x._3) }
  }





  implicit def parseAction(x :(ActionKeyword)): CsvAction = {
    x match { case `revrow` => RevAction(true) }
  }



  implicit def parseAction(dat: (DataKeyword, String)): DataOut = {
    dat._1 match { case `out` => DataOut(dat._2) }
  }
  // ----------------------  Impliciti za preverjanje formata ----------------

  // -------------------- Impliciti za izraze -------------

  def naturals:Stream[Int] = 0#::naturals.map(x=>x+1)

}


//  -------------------- Razredi za Nalogo 1. -------------

// Podatkovni viri
case class CsvData(data: Stream[Stream[String]], separator: Char) {

  def >>>(action: CsvAction): CsvData = action(this)

}



//Akcije
trait CsvAction {
  def apply(data: CsvData): CsvData = ???
  def -->(action: CsvAction): CsvAction = SeqAction(this, action)
}

// ta razred uporabite za zaporedje akcij
case class SeqAction(firstA: CsvAction, secondA: CsvAction) extends CsvAction {
  override def apply(data: CsvData): CsvData = data >>> firstA >>> secondA
}

case class RevAction(revCol: Boolean) extends CsvAction {

  def reverse(row :Stream[String]): Stream[String] = row.reverse

  override def apply(data: CsvData): CsvData = data match {
    case CsvData(stream, sep) => CsvData(stream.map(reverse), sep)
  } 
}

case class SwapCol(e1: Int, e2: Int) extends CsvAction {

  def swapCol(row: Stream[String]): Stream[String] = row.toList.updated(e1, row(e2)).updated(e2, row(e1)).toStream

  override def apply(csv: CsvData): CsvData = csv match {
    case CsvData(stream, sep) => CsvData(stream.map(swapCol), sep)
  }
}

case class DropCol(i: Int) extends CsvAction {
  override def apply(csv: CsvData): CsvData = {
    def dropCol(row: Stream[String]): Stream[String] = row.zip(CSVLang.naturals).filter(x => x._2 != i).map(x => x._1)

    csv match {
      case CsvData(stream, sep) => CsvData(stream.map(dropCol), sep)
    }
  }
                                
}

case class DataMerge(other: CsvData) extends CsvAction {
  override def apply(csv: CsvData): CsvData = (other,csv) match {
    case (CsvData(stream1, sep1), CsvData(stream2, sep2)) => CsvData(stream1#:::stream2, sep1)
  }
}

case class DataOut(filename: String) extends CsvAction {

  def printLine(row: Stream[String], pw: PrintWriter, sep: String) = pw.write(row.mkString(sep) + "\n")
  override def apply(csv: CsvData): CsvData = csv match {
    case CsvData(stream, sep) => {
      val pw = new PrintWriter(new File(filename))
      stream.foreach(x => printLine(x,pw, sep+""))
      pw.close
      csv
    }
  }
}

case class ScreenOut(sreenOut: Boolean) extends CsvAction {
  def printLine(row: Stream[String], sep:String) = println(row.mkString(sep)) 
  override def apply(csv: CsvData) = csv match {
    case CsvData(stream, sep) => {
      stream.foreach(x => printLine(x,sep+""))
      csv
    }
  }
}



//  -------------------- Razredi za Nalogo 2. -------------

case class FormatAction(f: CsvFormat) extends CsvAction {
  override def apply(csv: CsvData): CsvData = ???
}

case class ExpressionAction(expr: CsvExpr) extends CsvAction {
  override def apply(csv: CsvData): CsvData = ???
}

trait CsvFormat {
  // pomozni funkciji - uporabno za preverjanje formata
  def safeStringToInt(str: String): Option[Int] = {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toInt
  }
  def safeStringToDouble(str: String): Option[Double] = {
    import scala.util.control.Exception._
    catching(classOf[NumberFormatException]) opt str.toDouble
  }

  def |(other: CsvFormat): CsvFormat
  def checkAndMap(l: Stream[String]): Stream[String]
}

trait CsvExpr {
  def |+(other: CsvExpr): CsvExpr = ???
  def |*(other: CsvExpr): CsvExpr = ???
  def |/(other: CsvExpr): CsvExpr = ???
  def eval(line: Stream[String], accum: Double): Double = ???
}
