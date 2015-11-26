package main.scala

import _root_.ch.ethz.dal.tinyir.io.TipsterStream
import _root_.ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer, XMLDocument}
import scala.collection.mutable.{Map => MutMap}

object Main {

  type Features = Array[Array[Double]]
  type Labels   = Array[Int]

  val df = MutMap[String, Int]()
  val cf = MutMap[String, Int]()
  val idf = MutMap[String, Double]()

  val df_stem = MutMap[String, Int]()
  val idf_stem = MutMap[String, Double]()

  var collectionSize: Int = 200 // 242917
  val logCollectionSize = log2(collectionSize)
  val stopWords = StopWords.stopWords

  def log2(x: Double) = math.log10(x) / math.log10(2.0)

  def get_doc_frequency(docs: MutMap[String, XMLDocument]) = {
    /** Get document frequency and inverse document frequency. */
    for (doc <- docs) {
      df ++= Tokenizer.tokenize(doc._2.content.toLowerCase.trim()).distinct.map(t => t -> (1 + df.getOrElse(t, 0)))
      //cf ++= Tokenizer.tokenize(doc._2.content.toLowerCase.trim()).groupBy(identity).map(t => t._1 -> (t._2.length + cf.getOrElse(t._1, 0)))
    }

    df.foreach(kv => idf += kv._1 -> (logCollectionSize - log2(kv._2)))

    /*for (doc <- docs)
      df_stem ++= Tokenizer.tokenize(doc._2.content.toLowerCase.trim()).map(PorterStemmer.stem(_)).distinct.map(t => t -> (1 + df_stem.getOrElse(t, 0)))

    df_stem.foreach(kv => idf_stem += kv._1 -> (logCollectionSize - log2(kv._2)))*/
  }

  def testStream() = {

    val data_dir_path = "data/"

    // Load tipster articles
    val tipster = new TipsterStream(data_dir_path + "zips-1")
    println("Number of files in zips = " + tipster.length)

    // Load dataset of training topics and scores
    val topicsCollection: List[String] = io.Source.fromFile(data_dir_path + "topics").getLines().toList
    val scoresCollection: List[String] = io.Source.fromFile(data_dir_path + "qrels").getLines().toList
    val topics = MutMap[Int, String]()

    // Create map 'topic number -> topic title'
    topicsCollection.filter(p => p.startsWith("<num>")).foreach(f => topics +=
      topicsCollection(topicsCollection.indexOf(f)).replace(" ", "").takeRight(2).toInt -> topicsCollection(topicsCollection.indexOf(f) + 6)
        .replace("<title>", "").replace("Topic:", "").toLowerCase.trim())

    // get the name of documents found in the training set.
    val documentsInTrainingSet = scoresCollection.map(x => (x.split(" ").toList)(2).replace("-", "")).distinct

    // Load a small portion of the data (for debugging purposes, all collection will be further required)
    val subCollection = MutMap[String, XMLDocument]()

    /* add to subcollection only documents that are actually used for training. documents that don't appear in qrel are
       not helpful */
    var counter = 0

    val streamOfRelevantDocumentsForTraining = tipster.stream.take(collectionSize).filter( p => documentsInTrainingSet.contains(p.name))
    //val streamOfRelevantDocumentsForTraining = tipster.stream.take(collectionSize).filter( p => documentsInTrainingSet.contains(p.name))

    print("Number of relevant documents for training: ")
    //println(streamOfRelevantDocumentsForTraining.toList.length)

    //streamOfRelevantDocumentsForTraining.foreach{p => subCollection.put(p.name, p); if(counter % 100 == 0) println(counter); counter = counter + 1}
    println("Loading done")

    // Compute document and inversed document frequencies
    get_doc_frequency(subCollection)
    println("Frequencies computed")


    // Extract features from first 50 query-document pairs

    //val extracted = FeatureExtractor.extract_features(subCollection, scoresCollection, topics)
    val extracted = FeatureExtractor.extract_features(streamOfRelevantDocumentsForTraining, scoresCollection, topics)

    val features = extracted._1
    val labels = extracted._2

    // Perform classification using Random Forest from Weka ML library
    Classifier.train(features, labels)

    val predictedLabels = Classifier.predict(features)
    labels.zip(predictedLabels).foreach(kv => println(s"true\t${kv._1}, predicted\t${kv._2}"))

  }

  def main(args: Array[String]): Unit = {
    testStream()
  }

}
