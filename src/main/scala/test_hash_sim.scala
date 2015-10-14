package main.scala

/**
 * @author karimlabib
 */
object test_hash_sim {
  def main(args: Array[String]): Unit = {
    
    val ss = SimilarityDetector
    
    //val fp1 = ss.shinglesToFingerprint(Set(List("Karim"),List("Bahaa"), List("Rushdi")))
    //val fp2 = ss.shinglesToFingerprint(Set(List("Karim"),List("Bahaa"), List("Rushdi"), List("Labib")))
    
    //println(fp1.zip(fp2).map({case(a,b) => if(a==b) 1 else 0}).sum)
    
    //var x = (false,false)
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

    val url1 = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/en/the-eth-zurich/welcome-center/before-you-arrive/teaching-assistant.html"
    var(text1, links1) = Main_Object.getTextAndLinksFromUrl(url1)
    
    println(text1)
    
    val url2 = "http://idvm-infk-hofmann03.inf.ethz.ch/eth/www.ethz.ch/en/research.html"
    var(text2, links2) = Main_Object.getTextAndLinksFromUrl(url2)
    
    val x = ss.isSimilarOrDuplicate(text1.mkString(" "),url1)
    println(x)

    val y = ss.isSimilarOrDuplicate(text2.mkString(" "), url2)
    println(y)
    val doc = Main_Object
    
  }
}