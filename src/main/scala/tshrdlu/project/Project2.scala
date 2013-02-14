package tshrdlu.project

import twitter4j._
import tshrdlu.twitter._
import tshrdlu.util.{English,SimpleTokenizer,PolarityWords}
import java.util.zip.GZIPInputStream
import scala.collection.mutable
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
  //val TheRE = """(?i)\bthe\b""".r // Throw me away!
  def isEnglish(text: String):Boolean = {
    val words = text.split(" ").toList.filterNot(_.startsWith("@")).filterNot(_.startsWith("#"))
	val dict = new English
	var count = 0;
	for (word <- words) yield
	{
		if(dict.vocabulary.map(_.toLowerCase).filter(wd => wd.length > 1 ).contains(word.toLowerCase))
			count += 1
	}
	if(count > 2 && text.length > 2)
		return true;
	else if ( count > 0 && count == words.length)
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
object PolarityTermStreamer extends FilteredStreamer with TermFilter with PolarityStatusListener
{
	import tshrdlu.util.DecimalToPercent

	val termCounts = mutable.Map[String,Int]().withDefault(x=>0)
	val termPosCounts = mutable.Map[String,Int]().withDefault(x=>0)
	val termNegCounts = mutable.Map[String,Int]().withDefault(x=>0)
	val termNeutralCounts = mutable.Map[String,Int]().withDefault(x=>0)
	
	
	var input = Array[String]()
	
  override def main(args: Array[String]) {
    twitterStream.filter(getQuery(args))
	input = args
  }
	
	override def onStatus(status: Status) {
    val text = status.getText
	val terms = input.toList
    if (isEnglish(text)) {
      val polarityIndex = getPolarity(text)
      polarityCounts(polarityIndex) += 1
	  
      numEnglishTweets += 1
	  
	  //Update polarity and total counts for specified terms for filter
	  if(polarityIndex == 0)
			updateTermCounts(text,terms,termPosCounts,termCounts) 
	  else if(polarityIndex == 1)
			updateTermCounts(text,terms,termNegCounts,termCounts) 
	  else
			updateTermCounts(text,terms,termNeutralCounts,termCounts)
	 
	 
      println(polaritySymbol(polarityIndex) + ": " + text)

    if ((numEnglishTweets % outputInterval) == 0) {
		val posTerms = getPercents(termPosCounts,termCounts).toList
		val negTerms = getPercents(termNegCounts,termCounts).toList
		//val posTerm = termPosCounts.toList.sortBy(_._2).reverse
		//val negTerm = termNegCounts.toList.sortBy(_._2).reverse
		println("----------------------------------------")
		println("Number of English tweets processed: " + numEnglishTweets)
		println(polaritySymbol.mkString("\t"))
		println(polarityCounts.mkString("\t"))
		println(polarityCounts.map(_/numEnglishTweets).map(DecimalToPercent).mkString("\t"))
		print("Positive term percentages: ")
		println(posTerms.mkString(", "))
		print("Negative term percentages: ")
		println(negTerms.mkString(", "))
		println("\n----------------------------------------")
      }
    }
}
}
/**
 * Output polarity labels for every English tweet and output polarity
 * statistics at an interval of every ten tweets. Filtered by provided
 * query locations.
 */
object PolarityLocationStreamer extends FilteredStreamer with LocationFilter with PolarityStatusListener
{
	override val outputInterval = 10
}

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
  val polarityCheck = new PolarityWords
  def getPolarity(text: String): Int = {
    val words = text.split(" ").toList
	var pos = 0
	var neg = 0
	for(word <- words) yield
	{
		if(polarityCheck.stopwords.map(_.toLowerCase).contains(word.toLowerCase))
			pos += 1
		if(polarityCheck.vocabulary.map(_.toLowerCase).contains(word.toLowerCase))
			neg += 1
	}
	if(pos > neg)
		return 0;
	else if (neg > pos)
		return 1;
	else
		return 2;
  }

 /**
   * Helper function to update the counts for each term used in the PolarityTermStreamer:
   *   Inputs are the text of the tweet, the list of terms specified in the filter,
   *   the polarity map (pos,neg,neutral) to be increased when a term is found in the tweet,
   *   and the map that counts the total number of tweets each term occurs in
   */
  
  def updateTermCounts(text: String, terms: List[String], polMap: mutable.Map[String,Int], countMap: mutable.Map[String,Int]) ={
	val words = text.replaceAll("[^a-zA-Z ]", "").split(" ").toList
	for(term <- terms) yield
	{
		if(words.map(_.toLowerCase).contains(term.toLowerCase)){
			polMap(term) += 1
			countMap(term) += 1
		}
	}
  }
/**
   * Helper function to create a map that maps each filter term to its appropriate Polarity percentage used in the PolarityTermStreamer:
   *   Inputs are the polarity map (pos,neg,neutral) which will have the final count for each term for each sentiment 
   *   and the map that has the count for the total number of tweets each term occurs in
   *   The resulting percentage map, maps each filter term to the perentage value 
   */ 
 
  def getPercents(polarityCounts: mutable.Map[String,Int], termCounts: mutable.Map[String,Int]): mutable.Map[String,String] = {
	import tshrdlu.util.DecimalToPercent
	var percentages = mutable.Map[String,String]()
	
	for( term <- polarityCounts.keys.toList) yield {
		
		percentages(term) = DecimalToPercent(polarityCounts.get(term).getOrElse(1) / termCounts.get(term).getOrElse(1).toDouble)
	}
	return percentages
	
	
  }
}



