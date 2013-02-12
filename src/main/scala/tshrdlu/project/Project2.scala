package tshrdlu.project

import twitter4j._
import tshrdlu.twitter._
import tshrdlu.util.{English,SimpleTokenizer}
import java.util.zip.GZIPInputStream
/**
 * Show only tweets that appear to be English.
 */
object EnglishStatusStreamer extends BaseStreamer with EnglishStatusListener

/**
 * Debug the English detector.
 */
object EnglishStatusStreamerDebug extends BaseStreamer with EnglishStatusListenerDebug

/**
 * Output only tweets detected as English.
 */
trait EnglishStatusListener extends StatusOnlyListener {

  /**
   * If a status' text is English, print it.
   */
  override def onStatus(status: Status) { 
    val text = status.getText
    if (isEnglish(text)) 
      println(text)
  }

  /**
   * Test whether a given text is written in English.
   */
  val TheRE = """(?i)\bthe\b""".r // Throw me away!
  def isEnglish(text: String):Boolean = {
    val words = text.split(" ").toList
	val dict = new English
	var count = 0;
	var countStop = 0;
	for (word <- words) yield
	{
		if(dict.vocabulary.map(_.toLowerCase).filter(wd => wd.length > 1 ).contains(word.toLowerCase))
		{
			count += 1
		}
		if(dict.stopwords.map(_.toLowerCase).filter(wd => wd.length > 1).contains(word.toLowerCase))
		   countStop += 1
	}
	if(count > 2 && text.length > 2 && countStop > 1)
		return true;
	else if ( count > 0 && (count == text.length || countStop == text.length))
		return true;
	else	
		return false;
  }

}

/**
 * Output both English and non-English tweets in order to improve
 * the isEnglish method in EnglishStatusListener.
 */
trait EnglishStatusListenerDebug extends EnglishStatusListener {

  /**
   * For each status, print it's text, prefixed with the label
   * [ENGLISH] for those detected as English and [OTHER] for
   * those that are not.
   */
  override def onStatus(status: Status) { 
    val text = status.getText
    val prefix = if (isEnglish(text)) "[ENGLISH]" else "[OTHER]  "
    println(prefix + " " + text)
  }
}


/**
 * Output polarity labels for every English tweet and output polarity
 * statistics at default interval (every 100 tweets). Done for 
 * tweets from the Twitter sample.
 */
object PolarityStatusStreamer extends BaseStreamer with PolarityStatusListener

/**
 * Output polarity labels for every English tweet and output polarity
 * statistics at default interval (every 100 tweets). Filtered by provided
 * query terms.
 */
object PolarityTermStreamer 

/**
 * Output polarity labels for every English tweet and output polarity
 * statistics at an interval of every ten tweets. Filtered by provided
 * query locations.
 */
object PolarityLocationStreamer 


/**
 * For every tweet detected as English, compute its polarity based
 * on presence of positive and negative words, and output the label
 * with the tweet. Output statistics about polarity at specified
 * intervals (as given by the outputInterval variable).
 */
trait PolarityStatusListener extends EnglishStatusListener {

  import tshrdlu.util.DecimalToPercent

  val outputInterval = 100

  var numEnglishTweets = 0

  // Indices: 0 => +, 1 => -, 2 => ~
  val polaritySymbol = Array("+","-","~")
  var polarityCounts = Array(0.0,0.0,0.0)

  override def onStatus(status: Status) {
    val text = status.getText
    if (isEnglish(text)) {
      val polarityIndex = getPolarity(text)
      polarityCounts(polarityIndex) += 1

      numEnglishTweets += 1

      println(polaritySymbol(polarityIndex) + ": " + text)

      if ((numEnglishTweets % outputInterval) == 0) {
	println("----------------------------------------")
	println("Number of English tweets processed: " + numEnglishTweets)
	println(polaritySymbol.mkString("\t"))
	println(polarityCounts.mkString("\t"))
	println(polarityCounts.map(_/numEnglishTweets).map(DecimalToPercent).mkString("\t"))
	println("----------------------------------------")
      }
    }
    
  }

  
  /**
   * Given a text, return its polarity:
   *   0 for positive
   *   1 for negative
   *   2 for neutral
   */
  val random = new scala.util.Random
 // val positives = io.Source.fromInputStream(new GZIPInputStream(this.getClass.getResourceAsStream("/lang/eng/lexicon/positive-words.txt.gz"))).getLines.filterNot(_.startsWith(";")).toSet
  //val negatives = io.Source.fromInputStream(new GZIPInputStream(this.getClass.getResourceAsStream("/lang/eng/lexicon/negative-words.txt.gz"))).getLines.filterNot(_.startsWith(";")).toSet
  
  def getPolarity(text: String): Int = {
    val words = text.split(" ").toList
	var pos = 0
	var neg = 0
	val positives = io.Source.fromInputStream(new GZIPInputStream(this.getClass.getResourceAsStream("/lang/eng/lexicon/positive-words.txt.gz"))).getLines.filterNot(_.startsWith(";")).toSet
  val negatives = io.Source.fromInputStream(new GZIPInputStream(this.getClass.getResourceAsStream("/lang/eng/lexicon/negative-words.txt.gz"))).getLines.filterNot(_.startsWith(";")).toSet
  
	for(word <- words) yield
	{
		if(positives.map(_.toLowerCase).contains(word.toLowerCase))
		{
			pos += 1
		}
		if(negatives.map(_.toLowerCase).contains(word.toLowerCase))
		{
			neg += 1
		}
	}
	if(pos > neg)
		return 0;
	else if (neg > pos)
		return 1;
	else
		return 2;
  }

}



