import org.specs2.mutable._

class Week9Tests extends Specification {


	"Twitter" should {



		"isEnglish" in {
			val c = "sonček sonček sonček dan"
			val k = "danes je lep dan today is a nice day yes"
			val z = "sonček sonček sonček sonček today today today today"
			val f = "sonček sonček sonček sonček today today today"

			EnglishManipulator.isEnglish(c.split(" ").toList) mustEqual false
			EnglishManipulator.isEnglish(k.split(" ").toList) mustEqual true
			EnglishManipulator.isEnglish(z.split(" ").toList) mustEqual true
			EnglishManipulator.isEnglish(f.split(" ").toList) mustEqual false

		}

		"removePunc" in {
			SentimentManipulator.removePunc("hopa..!,,.") mustEqual "hopa"
			SentimentManipulator.removePunc(".......danes,!..!,.?") mustEqual "danes"
			SentimentManipulator.removePunc("") mustEqual ""
		}


		"isCommon" in {
			FreqManipulator.isCommon("that") mustEqual true
			FreqManipulator.isCommon("the") mustEqual true
			FreqManipulator.isCommon("ball") mustEqual false
			FreqManipulator.isCommon("today") mustEqual false

		}


		"sentimentValue" in {
			SentimentManipulator.sentimentValue(List("accidents", "accused")) mustEqual -4
			SentimentManipulator.sentimentValue(List("accidents", "accused", "sonček", "adorable")) mustEqual -1
			SentimentManipulator.removePunc("I'm!!!.") mustEqual "I'm"
			val tweet = "Go to hell, loser!!!"
			val l = tweet.split(" ").toList.map(x => SentimentManipulator.removePunc(x))
			SentimentManipulator.sentimentValue(l) mustEqual -7

			SentimentManipulator.sentimentValue(List("abhorrent")) mustEqual -3
			SentimentManipulator.sentimentValue(List("krnekaj")) mustEqual 0
		}
 	}


}
