package main.scala

import scala.collection.mutable.Queue

object Crawler {

  type Page = String

  var visited = Set[Page]()           // Set of already visited URLs
  var countExactDuplicates = 0        // Number of documents that are exact duplicates of other documents
  var countNearDuplicates = 0         // Number of documents that are near duplicates of other documents
  var countUniqueEnglish = 0          // Number of unique pages mostly written in English
  var countStudent = 0                // Case insensitive frequency of the term "student"

  var crawlQueue = Queue[Page]()


  /** Return true if the given URL is allowed to be crawled */
  def URLAllowed(url: String): Boolean = ???

  /** Start crawling at the specified URL */
  def start(url: String): Unit = {
    crawlQueue += url

    // Crawl through the whole queue
    while(!crawlQueue.isEmpty) {
      val page = crawlQueue.dequeue()

      // Crawl only if page hasn't been visited yet
      if(!visited.contains(page)) {
        visited = visited + page
        crawl(page)
      }
    }

    report()
  }

  /** Crawl a single URL. */
  def crawl(url: String): Unit = {

    // TODO make a GET request to URL, handling errors if necessary

    /* TODO do whatever we need with the DOM/text of the page we returned:
       - add all URLs found on the page to crawlQueue
       - detect language
       - calculate shingles/do something to allow detection of near and exact duplicates
       - count (case insensitive) occurrences of "student"
     */



  }

  /** Print a summary of the results of crawling */
  def report(): Unit = {
    println(s"Distinct URLs found: ${visited.size}")
    println(s"Exact duplicates found: $countExactDuplicates")
    println(s"Near duplicates found: $countNearDuplicates")
    println(s"Unique English pages found: $countUniqueEnglish")
    println(s"Term frequency of 'student': $countStudent")
  }

}
