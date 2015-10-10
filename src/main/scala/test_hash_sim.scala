package main.scala

/**
 * @author karimlabib
 */
object test_hash_sim {
  def main(args: Array[String]): Unit = {
    
    val ss = SimilarityDetector
    
    
    var x = ss.isSimilarOrDuplicate("Are the variables mutable in Java? If yes (else they should be final in Java): Would it make sense to make them immutable in Scala?")
    println(x)

    x = ss.isSimilarOrDuplicate("Are the variables orr mutable in Java? If yes (else they should be final in Java): Would it make sense to make them immutable in Scala?")
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
  }
}