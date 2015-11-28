package main.scala

import _root_.ch.ethz.dal.tinyir.processing.{TipsterCorpusIterator, StopWords, Tokenizer, XMLDocument}
import _root_.com.github.aztek.porterstemmer.PorterStemmer
import main.scala.Main._
import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap, Set => MutSet}

object FeatureExtractor {


  val df = MutMap[String, Int]()
  val cf = MutMap[String, Int]()
  val idf = MutMap[String, Double]()

  val df_stem = MutMap[String, Int]()
  val idf_stem = MutMap[String, Double]()

  var collectionSize: Int = 250000 // 242917
  val logCollectionSize = log2(collectionSize)
  val stopWords = StopWords.stopWords

  var queryTerms = MutSet[String]()

  def log2(x: Double) = math.log10(x) / math.log10(2.0)

  var documentCounter = 0

  var documentsInTrainingSet = Set[String]()

  // this map is used to keep the number of occurences of the query vocabulary in the document and keeps a map only
  // for the relevant documents
  val generalDocumentMapTermFrequency = MutMap[String, Map[String, Int]]()
  val generalDocumentMapTermFrequency_2 = MutMap[String, Map[String, Int]]() // half document
  val generalDocumentMapTermFrequency_5 = MutMap[String, Map[String, Int]]() // 1/5 of document

  def score_basic(query: String, doc: String) = {
    /** Number of terms in query*/
    val qterms = Tokenizer.tokenize(query).distinct

    def score(doc: List[String]): (Double, Double) = {

      val tfs: Map[String, Int] = doc.groupBy(identity).mapValues(l => l.length)
      val qtfs = qterms.flatMap(q => tfs.get(q))
      val numTermsInCommon = qtfs.length  // Number of query terms inside document
      val docLen = tfs.values.map(x => x * x).sum.toDouble
      val queryLen = qterms.length.toDouble
      val termOverlap = qtfs.sum.toDouble / (docLen * queryLen)


      //(numTermsInCommon.toDouble / queryLen + termOverlap, termOverlap)
      (0,0)
    }

    score(Tokenizer.tokenize(doc))
  }

  def score_tf_idf(query: String, doc: String, docName: String, pstemmer: Boolean) = {


    var qterms = Tokenizer.tokenize(query.toLowerCase).distinct.filter(!stopWords.contains(_))
    //val dterms = Tokenizer.tokenize(doc.toLowerCase)

    if (pstemmer) {
      qterms = qterms.map(PorterStemmer.stem(_))
      //dterms = dterms.map(PorterStemmer.stem(_))
    }

    //def tf(doc: List[String]): Map[String, Int] = doc.groupBy(identity).mapValues(l => l.length)
    def tf(docName: String): Map[String, Int] = generalDocumentMapTermFrequency.get(docName).get
    def tf_2(docName: String): Map[String, Int] = generalDocumentMapTermFrequency_2.get(docName).get
    def tf_5(docName: String): Map[String, Int] = generalDocumentMapTermFrequency_5.get(docName).get


    def logtf(tf: Map[String, Int]): Map[String, Double] = {
      val sum = tf.values.sum.toDouble
      //val sum = generalDocumentLength.get(docName).get
      tf.mapValues(v => log2((v.toDouble + 1.0) / sum))
    }

    var ltf = Map[String, Double]()
    var ltf_2 = Map[String, Double]()
    var ltf_5 = Map[String, Double]()

    /*
    qterms.map(q => ltf += q -> logtf(tf(dterms)).getOrElse(q, 0))
    qterms.map(q => ltf_2 += q -> logtf(tf(dterms.take(dterms.size / 2))).getOrElse(q, 0))
    qterms.map(q => ltf_5 += q -> logtf(tf(dterms.take(dterms.size / 5))).getOrElse(q, 0))
    */

    qterms.map(q => ltf += q -> logtf(tf(docName)).getOrElse(q, 0))
    qterms.map(q => ltf_2 += q -> logtf(tf_2(docName)).getOrElse(q, 0))
    qterms.map(q => ltf_5 += q -> logtf(tf_5(docName)).getOrElse(q, 0))

    val score1 = -1 * qterms.flatMap(q => ltf.get(q)).sum

    var score2 = 0.0
    var score3 = 0.0
    var score4 = 0.0

    if (!pstemmer) {
      score2 = -1 * qterms.map(q => (idf.getOrElse(q, 0.0) * ltf(q))).sum
      score3 = -1 * qterms.map(q => (idf.getOrElse(q, 0.0) * ltf_2(q))).sum
      score4 = -1 * qterms.map(q => (idf.getOrElse(q, 0.0) * ltf_5(q))).sum
    } else {
      score2 = -1 * qterms.map(q => (idf_stem.getOrElse(q, 0.0) * ltf(q))).sum
      score3 = -1 * qterms.map(q => (idf_stem.getOrElse(q, 0.0) * ltf_2(q))).sum
      score4 = -1 * qterms.map(q => (idf_stem.getOrElse(q, 0.0) * ltf_5(q))).sum
    }

    //(score1, score2, score3, score4)
    (score1, score2, score3, score4)
  }

  def score_title(query: String, docTitle: String) = {

    val qterms = Tokenizer.tokenize(query).distinct.map(PorterStemmer.stem(_))
    val length = qterms.length

    val titleTerms = Tokenizer.tokenize(docTitle).map(PorterStemmer.stem(_))
    val tfs: Map[String, Int] = titleTerms.groupBy(identity).mapValues(l => l.length)
    val qtfs = qterms.flatMap(q => tfs.get(q))

    qtfs.length.toDouble / length // percentage of terms in common

  }

  def get_doc_frequency(docs: Iterator[XMLDocument]) = {

    /** Get document frequency and inverse document frequency. */
    for(currentDocument <- docs) {

      if(documentCounter % 1000 == 0)
        println(documentCounter)

      documentCounter += 1

      val dterms = Tokenizer.tokenize(currentDocument.content.toLowerCase.trim())

      val allTokens = dterms
      val allTokens_2 = dterms.take(dterms.length/2)
      val allTokens_5 = dterms.take(dterms.length/5)

      val tokensFromQueryTerms = allTokens.filter(queryTerms.contains(_))
      val tokensFromQueryTerms_2 = allTokens_2.filter(queryTerms.contains(_))
      val tokensFromQueryTerms_5 = allTokens_5.filter(queryTerms.contains(_))

      val tmp = (tokensFromQueryTerms.groupBy(identity).map(t => t._1 -> (t._2.length)))
      generalDocumentMapTermFrequency += (currentDocument.name.replace("-","") ->  tmp)

      val tmp_2 = (tokensFromQueryTerms_2.groupBy(identity).map(t => t._1 -> (t._2.length)))
      generalDocumentMapTermFrequency_2 += (currentDocument.name.replace("-","") ->  tmp_2)

      val tmp_5 = (tokensFromQueryTerms_5.groupBy(identity).map(t => t._1 -> (t._2.length)))
      generalDocumentMapTermFrequency_5 += (currentDocument.name.replace("-","") ->  tmp_5)


      df ++= tokensFromQueryTerms.distinct.map(t => t -> (1 + df.getOrElse(t, 0)))
      cf ++= tokensFromQueryTerms.groupBy(identity).map(t => t._1 -> (t._2.length + cf.getOrElse(t._1, 0)))
    }

    df.foreach(kv => idf += kv._1 -> (logCollectionSize - log2(kv._2)))

    /*for (doc <- docs)
      df_stem ++= Tokenizer.tokenize(doc._2.content.toLowerCase.trim()).map(PorterStemmer.stem(_)).distinct.map(t => t -> (1 + df_stem.getOrElse(t, 0)))
    df_stem.foreach(kv => idf_stem += kv._1 -> (logCollectionSize - log2(kv._2)))*/
  }



  class FeatureArray(docName: String, featArr: Array[Double])
  {
    val correspondingDoc = docName
    val featureArray = featArr
  }

  implicit def orderedNode(featArr: FeatureArray): Ordered[FeatureArray] = new Ordered[FeatureArray] {
    def compare(other: FeatureArray) =
    {
      -1 * featArr.featureArray(4).compare(other.featureArray(4)) // normal tf-idf is in index 4 of the feature vector
    }
  }

  var best1000FeaturesForRanking = List[mutable.PriorityQueue[FeatureArray]]() // based on tf-idf

  var featureVectorsUsedForTraining = Array[Array[Double]]()
  var labelsForTraining = Array[Int]()

  def second_pass(docs: Iterator[XMLDocument], topics: MutMap[Int, String], scoresCollectionSorted: List[List[String]]) =
  {
    val numOfTopics = 40

    for(i <- 0 to numOfTopics - 1)
      {
        best1000FeaturesForRanking = best1000FeaturesForRanking.+:(new mutable.PriorityQueue[FeatureArray]())
      }

    var qrel_counter = 0

    documentCounter = 0

    val all_topics_sorted = topics.toList.sortWith(_._1 < _._1)

    for(doc <- docs)
      {
        if(documentCounter % 1000 == 0)
          println(documentCounter)

        documentCounter += 1

        val doc_content = doc.content.toLowerCase.trim()
        val doc_title = doc.title.toLowerCase.trim()
        val doc_name = doc.name.trim().replace("-","")

        var topic_counter = -1

        for(topic <- all_topics_sorted)
          {
            if(topic_counter <= 1) {
              topic_counter += 1

              val query_title = topic._2

              val score1 = score_basic(query_title, doc_content)

              val score2 = score_title(query_title, doc_title)

              val score3 = score_tf_idf(query_title, doc_content, doc_name, false)

              val feature_array = Array(score1._1, score1._2, score2, score3._1, score3._2, score3._3, score3._4)
              best1000FeaturesForRanking(topic_counter).enqueue(new FeatureArray(doc_name, feature_array))

              if (best1000FeaturesForRanking(topic_counter).size == 1001) {
                // keep only best 1000 features..
                best1000FeaturesForRanking(topic_counter).dequeue()
              }

              if (scoresCollectionSorted(qrel_counter)(0).toInt == (topic_counter + 51) && scoresCollectionSorted(qrel_counter)(2).replace("-", "").equals(doc_name)) {
                // current query - document pair is in qrel
                val relevance = scoresCollectionSorted(qrel_counter)(3).toInt

                featureVectorsUsedForTraining = featureVectorsUsedForTraining :+ Array(score1._1, score1._2, score2, score3._1, score3._2, score3._3, score3._4, relevance)
                labelsForTraining = labelsForTraining :+ relevance

                qrel_counter += 1
              }
            }
          }

      }

  }


  def extract_features(): (Features, Labels) = {

    val data_dir_path = "data/"

    // Load dataset of training topics and scores
    val topicsCollection: List[String] = io.Source.fromFile(data_dir_path + "topics").getLines().toList
    val scoresCollection: List[String] = io.Source.fromFile(data_dir_path + "qrels").getLines().toList
    val topics = MutMap[Int, String]()

    // Create map 'topic number -> topic title'
    topicsCollection.filter(p => p.startsWith("<num>")).foreach(f => topics +=
      topicsCollection(topicsCollection.indexOf(f)).replace(" ", "").takeRight(2).toInt -> topicsCollection(topicsCollection.indexOf(f) + 6)
        .replace("<title>", "").replace("Topic:", "").toLowerCase.trim())


    /****************** FIRST PASS **********/

    documentsInTrainingSet = scoresCollection.map(x => (x.split(" ").toList)(2).replace("-", "")).distinct.toSet

    topics.foreach(t => queryTerms ++= Tokenizer.tokenize(t._2).distinct.filter(!StopWords.stopWords.contains(_)))

    println(queryTerms)

    var tipster = new TipsterCorpusIterator(data_dir_path + "allZips")

    get_doc_frequency(tipster)

    println("Frequencies computed...")

    generalDocumentMapTermFrequency.foreach{ case p => println(); println(p._1); p._2.foreach{case m => println(m._1+ " " + m._2) } }


    println("Processed documents for first time: " + documentCounter)

    /********* FINISHED FIRST PASS **********/


    /************** SECOND PASS *************/

    tipster = new TipsterCorpusIterator(data_dir_path + "allZips")

    val scoresCollectionSorted = scoresCollection.map(s => s.split(" ").toList.map(e => e.replace("-", ""))).sortWith(_(2) < _(2))

    second_pass(tipster, topics, scoresCollectionSorted)

    /************** SECOND PASS *************/

    return (featureVectorsUsedForTraining, labelsForTraining)
  }

}
