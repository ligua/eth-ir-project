package main.scala

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import scala.collection.mutable.{Set => MutSet}
import scala.collection.mutable.{Stack => MutStack}
import scala.collection.mutable.{Queue => MutQueue}
import org.jsoup.select.Evaluator.IsEmpty
import java.io._

object Main_Object {

  val SimDetector = SimilarityDetector
  val LangDetector = LanguageDetector
  
  var visitedUrls = MutSet[String]()
  var FrontierUrlsToBeVisited = MutQueue[String]()
  var allEnglishUrls = List[String]()
  
  var studentCount = 0L
  var uniqueEnglishCount = 0L

  var weirdLinks = List[String]()
  
  /** Tokenize a document for similarity detection */
  def tokenize(doc: String): List[String] = {
    doc.toLowerCase.split("[ .,;:?!\t\n\r\f]+").toList
  }
  
  def getURLName(name: String, relativeReturn: Int) : String = {

    val splitted = name.split("/");

    if(splitted.length-1-relativeReturn >= -1) 
      splitted.take(splitted.length-1-relativeReturn).reduceLeft(_.+("/").+(_)).+("/")
    else ""
  }

  def getURLDomain(name: String) : String = {

    val splitted = name.split("/");

    splitted(2)
  }

  def getAllLinks(doc: Document): List[String] =
  {
    val elements = doc.getElementsByTag("a")
    var allLinks = List[String]()

    for(i <- 0 to elements.size()-1)
    {
      val s = elements.get(i).attr("href")
      
      if(s.endsWith(".html"))
      {
        
        if(!s.contains("login") && !s.contains("http") && !s.contains("#") && !s.contains("..") && !s.contains(".png") && !s.contains(".pdf"))
        {
          if(doc.location.endsWith("/"))
          {
            allLinks = allLinks.:+(doc.location().+(s))
          }
          else 
          {
            allLinks = allLinks.:+(getURLName(doc.location(), 0).+(s))
          }
        }
        
        if(s.contains(".."))
        {
          if(!s.contains("login"))
          {
            val splitUrl = s.split("/")
            
            var relativeReturnCount = 0
            
            splitUrl.foreach { x => if(x == "..") relativeReturnCount = relativeReturnCount + 1 }
            
            val urlName = getURLName(doc.location(), relativeReturnCount)
            
            if(!urlName.isEmpty())
            {
              val pageName = splitUrl.takeRight(splitUrl.size-relativeReturnCount).reduceLeft(_.+("/").+(_))
              allLinks = allLinks.:+(urlName.+(pageName))
            }
          }
        }
      }
    }

    allLinks
  }
  
  def getAllText(doc: Document): List[String] =
  {
    var allText = List[String]()
    
    //if(doc.location().endsWith(".html"))
    //{
      var mainContentElementDiv = doc.getElementById("mainContent")
      if(mainContentElementDiv == null)
        mainContentElementDiv = doc.getElementById("contentMain")
        
      var allElements = doc.getAllElements
      
      mainContentElementDiv = null
      
      if(mainContentElementDiv != null)
        allElements = mainContentElementDiv.getAllElements();
  
      for(i <- 0 to allElements.size()-1)
      {
        if(allElements.get(i).hasText())
        {
          val text = allElements.get(i).ownText().trim()
          if(text.length() > 0)
            allText = allText.:+(text)
        }
      }
    //}

    allText
  }

  def getDocumentFromUrl(url: String): Document =
  {
      Jsoup.connect(url).get
  }
  
  def getTextAndLinksFromUrl(url: String): (List[String], List[String]) = 
  {
    val doc = getDocumentFromUrl(url)
    
    (getAllText(doc).mkString(" ").filter(!",;.!?".contains(_)).split(" ").toList,
     getAllLinks(doc))
  }
  
  def crawlUrl(url: String)
  {
    visitedUrls.add(url)
    
    FrontierUrlsToBeVisited.enqueue(url)
    
    var counterToPrint = 0;
    
    while(!FrontierUrlsToBeVisited.isEmpty)
    {
      val currentUrl = FrontierUrlsToBeVisited.dequeue()
        
      try
      {
        val (allText, allLinks) = getTextAndLinksFromUrl(currentUrl)
  
        counterToPrint = (counterToPrint+1)
      
        if(counterToPrint%20 == 0)
        {
          println("Crawled: "+counterToPrint+", RemainingToCrawl: "+FrontierUrlsToBeVisited.size)
          println("exact found: " + SimDetector.getNumOfExactAndNearDuplicates()._1)
          println("near found: " + SimDetector.getNumOfExactAndNearDuplicates()._2)
        }
  
        val(isExactDuplicate, isNearDuplicate) = SimDetector.isSimilarOrDuplicate(allText.mkString(" "),currentUrl)

        // Detect language
        if(!isExactDuplicate && !isNearDuplicate) {
          val lang = LangDetector.detect(allText.mkString((" ")))
          if(lang == "en")
          {
            studentCount = studentCount + tokenize(allText.mkString(" ")).filter(_.toLowerCase().equals("student")).size
            
            uniqueEnglishCount += 1
            allEnglishUrls = allEnglishUrls.:+(currentUrl)
          }
        }
        
        for(link <- allLinks)
        {
          if(!visitedUrls.contains(link))
          {
            visitedUrls.add(link)
            FrontierUrlsToBeVisited.enqueue(link)
          }
        }
        
      }
      catch
      {
        case e: NullPointerException => //println("NULL PT EXCEPTION")
        case e: org.jsoup.HttpStatusException => visitedUrls.remove(currentUrl) // page is not found, so remove it from visited urls
        case e: java.net.SocketTimeoutException => //println("Soket TimeOutException")
        case e: Exception => println(e.getMessage)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    
    val startSeed = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/en.html"
    crawlUrl(startSeed)
    
    val pw = new PrintWriter(new File("EnglishUrls.txt"));
    allEnglishUrls.sorted.foreach(pw.println(_))
    pw.close();
    
    val pwVisUrls = new PrintWriter(new File("VisitedUrls.txt"));
    visitedUrls.toList.sorted.foreach(pwVisUrls.println(_))
    pwVisUrls.close();
    
    val (exactDuplicates, nearDuplicates) = SimDetector.getExactAndNearDuplicates
    
    val pwExact = new PrintWriter(new File("ExactDuplicates.txt"));
    exactDuplicates.foreach{case (x,y) => pwExact.println(x+"\n"+y+"\n\n")}
    pwExact.close();
    
    val pwNear = new PrintWriter(new File("NearDuplicates.txt"));
    nearDuplicates.foreach{case (x,y) => pwNear.println(x+"\n"+y+"\n\n")}
    pwNear.close();
    
    
    
    val duplicates = SimDetector.getNumOfExactAndNearDuplicates()
    
    println("Distinct URLs found: ".+(visitedUrls.size))
    println("Exact duplicates found: ".+(duplicates._1))
    println("Near duplicates found: ".+(duplicates._2))
    println("Unique English pages found: ".+(uniqueEnglishCount))
    println("Term frequency of \"student\": ".+(studentCount))
  }
}