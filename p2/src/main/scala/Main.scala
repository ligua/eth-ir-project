package main.scala

import ch.ethz.dal.tinyir.io.TipsterStream

object Main {

  // Define useful types
  type Document = String
  type Features = List[Double]
  type Label    = Boolean

  def testStream() = {
    /** Show some test statistics about the dataset we have. */
    val tipster = new TipsterStream ("data/zips")
    println("Number of files in zips = " + tipster.length)

    var length : Long = 0
    var tokens : Long = 0
    for (doc <- tipster.stream.take(10000)) {
      length += doc.content.length
      tokens += doc.tokens.length
    }
    println("Final number of characters = " + length)
    println("Final number of tokens     = " + tokens)
  }

  def main(args: Array[String]): Unit = {

    // testStream()

    // Create a document stream and extract features from it
    val docStream = new TipsterStream("data/zips")
    val featureStream: Stream[Features] = FeatureExtractor.extract(docStream)

    


  }

}