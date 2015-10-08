package Main.scala

import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import scala.collection.mutable.{Set => MutSet}
import scala.collection.mutable.{Set => MutSet}

object Main_Object {

  var visitedUrls = MutSet[String]()
  var studentCount = 0L

  def getURLName(name: String) : String = {

    val splitted = name.split("/");

    splitted.take(splitted.length-1).reduceLeft(_.+("/").+(_)).+("/")
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

      if(!s.contains(".html.tmp") && !s.contains("http") && !s.contains("#") && !s.contains("..") && !s.contains("png"))
      {
        allLinks = allLinks.+:(getURLName(doc.location()).+(s))
      }
    }

    allLinks
  }

  def getAllText(doc: Document): List[String] =
  {
    val allElements = doc.getAllElements();

    var allText = List[String]()

    for(i <- 0 to allElements.size()-1)
    {
      if(allElements.get(i).hasText())
      {
        val text = allElements.get(i).ownText().trim()
        if(text.length() > 0)
          allText = allText.+:(text)
      }
    }

    allText
  }

  def crawlUrl(url: String)
  {
    visitedUrls.add(url)
    println(url);
    if(visitedUrls.size % 100 == 0)
      println(visitedUrls.size)
    try
    {
      val doc = Jsoup.connect(url).get
      val allLinks = getAllLinks(doc)

      val allText = getAllText(doc).mkString(" ").filter(!",;.!?".contains(_)).split(" ")
      //allText.filter(_.toLowerCase().contains("student")).foreach(_ => studentCount += 1)

      for(link <- allLinks)
      {

        if(!visitedUrls.contains(link))
          crawlUrl(link)
      }

    }
    catch {
      case e: Exception => ()
    }
  }

  def main(args: Array[String]): Unit = {


    val startSeed = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/en.html"
    crawlUrl(startSeed)

    println(visitedUrls.size)
    println("Term frequency of \"student\": ".+(studentCount))
  }
}