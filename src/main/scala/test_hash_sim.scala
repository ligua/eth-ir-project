package main.scala

/**
 * @author karimlabib
 */
object test_hash_sim {
  def main(args: Array[String]): Unit = {
    
    val ss = SimilarityDetector
    
    var x = (false,false)
    /*
    var x = ss.isSimilarOrDuplicate("Are the variables mutable in Java? If yes (else they should be final in Java): Would it make sense to make them immutable in Scala?")
    println(x)
    
    x = ss.isSimilarOrDuplicate("Are the variables orr mutable in Java? If yes (else they should be final in Java): Would it make sense to make them immutable in Scala?")
    println(x)
    
    x = ss.isSimilarOrDuplicate("Why are are the variables private? Do they have getters and/or setters?")
    println(x)
    
    x = ss.isSimilarOrDuplicate("Why are are the variables private? Do they have getters and/or setters?")
    println(x)
    
    x = ss.isSimilarOrDuplicate("Shakespeare produced most of his known work between 1589 and 1613")
    println(x)
    
    x = ss.isSimilarOrDuplicate("Shakespeare produced most of his work after 1589")
    println(x)
    * 
    *
    */
    
    val main_obj = Main_Object

    val url1 = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/students/en/service/it-services.html"
    var(text1, links1) = Main_Object.getTextAndLinksFromUrl(url1)
    
    val url2 = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/students/en/continuing-education/withdrawal-cancellation.html"
    var(text2, links2) = Main_Object.getTextAndLinksFromUrl(url2)
    
    x = ss.isSimilarOrDuplicate(text1.mkString(" "),url1)
    println(x)

    x = ss.isSimilarOrDuplicate(text2.mkString(" "), url2)
    println(x)
    val doc = Main_Object
    
  }
}