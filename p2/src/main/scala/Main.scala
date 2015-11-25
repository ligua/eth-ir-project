package main.scala

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer, XMLDocument}
import scala.collection.mutable.{Map => MutMap}

object Main {

  type Features = Array[Array[Double]]
  type Labels   = Array[Int]

  val df = collection.mutable.Map[String, Int]()
  val idf = collection.mutable.Map[String, Double]()

  val df_stem = collection.mutable.Map[String, Int]()
  val idf_stem = collection.mutable.Map[String, Double]()

  var collectionSize: Int = 1
  val logCollectionSize = log2(collectionSize)
  val stopWords = StopWords.stopWords

  def log2(x: Double) = math.log10(x) / math.log10(2.0)

  def get_doc_frequency(docs: scala.collection.mutable.Map[String, XMLDocument]) = {
    for (doc <- docs)
      df ++= Tokenizer.tokenize(doc._2.content.toLowerCase.trim()).distinct.map(t => t -> (1 + df.getOrElse(t, 0)))

    df.foreach(kv => idf += kv._1 -> (logCollectionSize - log2(kv._2)))

    /*for (doc <- docs)
      df_stem ++= Tokenizer.tokenize(doc._2.content.toLowerCase.trim()).map(PorterStemmer.stem(_)).distinct.map(t => t -> (1 + df_stem.getOrElse(t, 0)))

    df_stem.foreach(kv => idf_stem += kv._1 -> (logCollectionSize - log2(kv._2)))*/
  }

  def testStream() = {

    // Load tipster articles
    val tipster = new TipsterStream("data/zips")
    println("Number of files in zips = " + tipster.length)

    //Load dataset of training topics and scores
    val topicsCollection: List[String] = io.Source.fromFile("data/topics").getLines().toList
    val scoresCollection: List[String] = io.Source.fromFile("data/qrels").getLines().toList
    val topics = MutMap[Int, String]()

    topicsCollection.filter(p => p.startsWith("<num>")).foreach(f => topics +=
      topicsCollection(topicsCollection.indexOf(f)).replace(" ", "").takeRight(2).toInt -> topicsCollection(topicsCollection.indexOf(f) + 6)
        .replace("<title>", "").replace("Topic:", "").toLowerCase.trim())

    // Load a small portion of the data (for debugging purposes, all collection will be further required)
    val subCollection = MutMap[String, XMLDocument]()
    tipster.stream.take(collectionSize).foreach(p => subCollection.put(p.name, p))
    println("Loading done")

    // Compute document and inversed document frequencies
    get_doc_frequency(subCollection)
    println("Frequencies computed")


    // Extract features from first 50 query-document pairs
    val extracted = FeatureExtractor.extract_features(subCollection, scoresCollection, topics)
    val features = extracted._1
    val labels = extracted._2

    // Perform classification using Random Forest from Weka ML library
    Classifier.train(features, labels)

  }

  def main(args: Array[String]): Unit = {
    testStream()
  }

}
