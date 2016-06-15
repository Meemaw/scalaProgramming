import twitter4j._
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import scala.io._
import scalax.chart.api._

//Tega razreda ni potrebno spreminjati, samo dodajte svoje ključe iz Twitterja
class TweetStreamer(manipulator: TweetManipulator) {
  val config = new twitter4j.conf.ConfigurationBuilder()
    .setOAuthConsumerKey("FtiwSLnEDmrzNvotmYJPjijC1") //sem vpišite 4 "ključe"
    .setOAuthConsumerSecret("mFaUqPQI2zMOU3c0WIFJdtggakqv2yTVsVle50kserVOXR0Mcg")
    .setOAuthAccessToken("3921389776-RqGNPA1bUrFchPU94D7YWKBUW9Y5uLm46JrjU7T")
    .setOAuthAccessTokenSecret("n9HAMIB26IuUogmTfR9Iavg5RsOINqrld1OsMCfrpSL8F")
    .build

  def simpleStatusListener = new StatusListener() {
    def onStatus(status: Status) {
      val tweet = status.getText
      manipulator.process(tweet)
    }
    def onDeletionNotice(statusDeletionNotice: StatusDeletionNotice) {}
    def onTrackLimitationNotice(numberOfLimitedStatuses: Int) {}
    def onException(ex: Exception) { ex.printStackTrace }
    def onScrubGeo(arg0: Long, arg1: Long) {}
    def onStallWarning(warning: StallWarning) {}
  }

}

trait TweetManipulator {
  def process(tweet: String)
  def outputFinal
}

//Primer obdelovalca tweetov
object TrivialManipulator extends TweetManipulator {
  def process(tweet: String) = {
    println("-------------------")
    println(tweet)
  }
  def outputFinal = println("Finished streaming")
}

//Primer enostavne naloge
object NalogaDemo {
  def main(args: Array[String]) {
    val tS = new TweetStreamer(TrivialManipulator)
    val twitterStream = new TwitterStreamFactory(tS.config).getInstance
    twitterStream.addListener(tS.simpleStatusListener)
    twitterStream.sample
    Thread.sleep(10000)
    TrivialManipulator.outputFinal
    twitterStream.cleanUp
    twitterStream.shutdown
  }
}

// Naloga 1
object EnglishManipulator extends TweetManipulator {
  lazy val dic = Source.fromFile("resources/wordsEn.txt").getLines.toList

  def isEnglish(l: List[String]): Boolean = {
      l.length / 2 <= l.count(p => dic.contains(p.toLowerCase))
  }

  def isEnglish(word: String):Boolean = dic.contains(word)

  // my eyes hurt by all the variables :(
  var numTweets = 0
  var numEnTweets = 0

  def process(tweet: String) = {
    val l = tweet.split("\\s+").toList.map(x => SentimentManipulator.removePunc(x))
    if(isEnglish(l)) {
      println("-------------------")
      println(tweet)
      numTweets = numTweets + 1
      numEnTweets = numEnTweets + 1
    }
    else {
      numTweets = numTweets + 1
    }
  }

  // izpišemo razmerje angleških tweetov
  def outputFinal = {
    println("Number of English Tweets: " + numEnTweets)
    println("Number of all Tweets: " + numTweets)
    println("Ratio of English Tweets: " + (numEnTweets.toDouble / numTweets)*100 + "%")
  }
}

object Naloga1 {

  def main(args: Array[String]) {
    val tS = new TweetStreamer(EnglishManipulator)
    val twitterStream = new TwitterStreamFactory(tS.config).getInstance
    twitterStream.addListener(tS.simpleStatusListener)
    twitterStream.sample
    Thread.sleep(10000)
    EnglishManipulator.outputFinal
    twitterStream.cleanUp
    twitterStream.shutdown

  }
}

// Naloga 2
object FreqManipulator extends TweetManipulator {
  lazy val commons = Source.fromFile("resources/commonEng.txt").getLines.toList
  var myMap = HashMap[String,Int]()

  def isCommon(w: String): Boolean = commons.contains(w)
  // sortiramo po pogostosti in vzamemo 100 elementov
  def mostCommon: List[(String, Int)] = myMap.toArray.sortBy(_._2).toList.reverse.take(100)
  def process(tweet: String) = {
    val l = tweet.split("\\s+").toList.map(x => SentimentManipulator.removePunc(x))
    // gremo skozi vse besede tweeta in preverimo da je angleška, ni coommon in da je daljša od 3
    for (word <- l if EnglishManipulator.isEnglish(word) && !isCommon(word) && word.length >= 3) {
      if(myMap.contains(word)) myMap.put((word), myMap(word)+1)
      else myMap.put(word, 1)
    } 
  }
  def outputFinal = {
    mostCommon.foreach(println)
  }
}

object Naloga2 {
  def main(args: Array[String]) {
    val tS = new TweetStreamer(FreqManipulator)
    val twitterStream = new TwitterStreamFactory(tS.config).getInstance
    twitterStream.addListener(tS.simpleStatusListener)
    twitterStream.sample
    Thread.sleep(10000)
    FreqManipulator.outputFinal
    twitterStream.cleanUp
    twitterStream.shutdown
  }
}

// Naloga 3
object SentimentManipulator extends TweetManipulator {

  val startingTime = System.nanoTime()

  // parsanje fajla v list tuplov
  lazy val sentiments:List[(String,Int)] = {
    Source.fromFile("resources/sentiment.txt").getLines.toList.map(x => {
    val split = x.split("\\s+") match {
      case Array(str1,str2) => (str1,str2.toInt)
      case Array(str1,str2,str3) => (str1+str2, str3.toInt)
      case Array(str1,str2,str3,str4) => (str1+str2+str3,str4.toInt)
      case _ => throw new Exception
    }
    split
    })
  }

  // parsanje list tuplov v hashmap
  def buildMap:HashMap[String,Int] = {
    var myMap = HashMap[String,Int]()
    for(t <- sentiments) {
      myMap.put(t._1,t._2)
    }
    myMap
  }

  lazy val myMap = buildMap

  def removePunc(s:String):String = s.replaceAll("""[\p{Punct}&&[^']]""", "")
  def sentimentValue(tweet: List[String]):Int = tweet.foldLeft(0)((acc, c) => if(myMap.contains(c)) acc + myMap(c) else acc)
  def average(c:List[Int]):Double = c.foldLeft(0)((acc,c) => acc +c) / c.length.toDouble
  var chartMap = HashMap[Int,List[Int]]()

  def process(tweet: String) = {
    val l = tweet.split(" ").toList.map(x => removePunc(x))
    // procesira samo angleške tweete (da ni toliko nevtralnih besed, posledično ima graf malo večje vrednosti)
    if(EnglishManipulator.isEnglish(l)) {
      val curent = sentimentValue(l)
      val seconds = (System.nanoTime() - startingTime) / 1000000000.0
      val minutes = (seconds / 60).toInt
      if(chartMap.contains(minutes)) chartMap.put(minutes, (chartMap(minutes)):::List(curent))
      else chartMap.put(minutes, List(curent))
      }
    
  }

  // pripravimo list tuplov (minuta, povprečna vrednost v minuti)
  def pripraviIzpis(map:HashMap[Int,List[Int]]):List[(Int,Double)] = {
    map.toList.map(x => (x._1.toInt, BigDecimal(average(x._2)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)).toArray.sortBy(_._1).toList
  }

  def outputFinal = {
    val izpis = pripraviIzpis(chartMap)
    val chart = XYLineChart(izpis)

    chart.saveAsPNG("resources/chart.png")
    chart.show()
  }
}

object Naloga3 {
  def main(args: Array[String]) {
    val tS = new TweetStreamer(SentimentManipulator)
    val twitterStream = new TwitterStreamFactory(tS.config).getInstance
    twitterStream.addListener(tS.simpleStatusListener)
    twitterStream.sample
    Thread.sleep(3600000)
    SentimentManipulator.outputFinal
    twitterStream.cleanUp
    twitterStream.shutdown
  }
}
