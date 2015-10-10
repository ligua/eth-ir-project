package main.scala

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import scala.collection.mutable.{Set => MutSet}
import scala.collection.mutable.{Stack => MutStack}
import org.jsoup.select.Evaluator.IsEmpty

object Main_Object {

  val SimDetector = SimilarityDetector
  
  var visitedUrls = MutSet[String]()
  var stackUrlsToBeVisited = MutStack[String]()
  
  var studentCount = 0L

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
      val t = elements.get(i).text()

      if(s.endsWith(".html"))
      {
        if(!s.contains("login") && !s.contains("http") && !s.contains("#") && !s.contains(".."))
        {
          allLinks = allLinks.:+(getURLName(doc.location(), 0).+(s))
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
    var mainContentElementDiv = doc.getElementById("mainContent")
    if(mainContentElementDiv == null)
      mainContentElementDiv = doc.getElementById("contentMain")
      
    var allElements = doc.getAllElements
    
    if(mainContentElementDiv != null)
      allElements = mainContentElementDiv.getAllElements();
    
    var allText = List[String]()

    for(i <- 0 to allElements.size()-1)
    {
      if(allElements.get(i).hasText())
      {
        val text = allElements.get(i).ownText().trim()
        if(text.length() > 0)
          allText = allText.:+(text)
      }
    }

    allText
  }

  def getDocumentFromUrl(url: String): Document =
  {
      Jsoup.connect(url).get
  }
  
  def getTextAndLinksFromUrl(url: String): (List[String], List[String]) = 
  {
    val doc = getDocumentFromUrl(url)
    
    (getAllText(doc).mkString(" ").filter(!",;.!?".contains(_)).split(" ").toList, getAllLinks(doc))
  }
  
  def crawlUrl(url: String)
  {
    visitedUrls.add(url)
    println(url);
    
    stackUrlsToBeVisited.push(url)
    
    try
    {
      while(!stackUrlsToBeVisited.isEmpty)
      {
        try
        {
          val currentUrl = stackUrlsToBeVisited.pop()
          
          val (allText, allLinks) = getTextAndLinksFromUrl(currentUrl)
    
          studentCount = studentCount + allText.filter(_.toLowerCase().equals("student")).size
    
          val(isExactDuplicate, isNearDuplicate) = SimDetector.isSimilarOrDuplicate(allText.mkString(" "),currentUrl)
          
          for(link <- allLinks)
          {
            if(!visitedUrls.contains(link))
            {
              visitedUrls.add(link)
              stackUrlsToBeVisited.push(link)
            }
          }
        }
        catch
        {
          case e: NullPointerException => //println("NULL PT EXCEPTION")
          case e: org.jsoup.HttpStatusException => //println("HTTP status exception")
          case e: java.net.SocketTimeoutException => //println("Soket TimeOutException")
          //case e: Exception => println("SOMETHING ELSE")
        }
        
      }
    }
  }

  def main(args: Array[String]): Unit = {


    val startSeed = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/en.html"
    crawlUrl(startSeed)
    val duplicates = SimDetector.getNumOfExactAndNearDuplicates()
    
    println("Distinct URLs found: ".+(visitedUrls.size))
    println("Exact duplicates found: ".+(duplicates._1))
    println("Near duplicates found: ".+(duplicates._2))
    println("Term frequency of \"student\": ".+(studentCount))
  }
}