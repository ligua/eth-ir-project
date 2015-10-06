package main.scala



object Crawler {

  var visited = Set()                 // Set of already visited URLs
  var countExactDuplicates = 0        // Number of documents that are exact duplicates of other documents
  var countNearDuplicates = 0         // Number of documents that are near duplicates of other documents
  var countUniqueEnglish = 0          // Number of unique pages mostly written in English
  var countStudent = 0                // Case insensitive frequency of the term "student"

  var crawlQueue = List()


  /** Return true if the given URL is allowed to be crawled */
  def URLAllowed(url: String): Boolean = ???

  /** Start crawling at the specified URL */
  def start(url: String): Unit = ???

  /** Crawl a single URL. */
  def crawl(url: String): Unit = ???

  /** Print a summary of the results of crawling */
  def report(): Unit = {
    println(s"Distinct URLs found: ${visited.size}")
    println(s"Exact duplicates found: $countExactDuplicates")
    println(s"Near duplicates found: $countNearDuplicates")
    println(s"Unique English pages found: $countUniqueEnglish")
    println(s"Term frequency of 'student': $countStudent")
  }

}
