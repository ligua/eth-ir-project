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

  var countTotalTermsInAllDocs = 0

  var collectionSize: Int = 0

  val stopWords = StopWords.stopWords

  var queryTerms = MutSet[String]()

  def log2(x: Double) = math.log10(x) / math.log10(2.0)

  var documentCounter = 0

  var documentsInTrainingSet = Set[String]()
  var scoresCollectionSorted = List[List[String]]()

  var documentQrelMap = MutMap[String, MutMap[Int, Int]]() // For each doc we have a map (topic -> relevance), e.g. (51 -> 1)

  val languageModelResultLists = MutMap[Int, mutable.PriorityQueue[LanguageModelResult]]()


  // this map is used to keep the number of occurences of the query vocabulary in the document and keeps a map only
  // for the relevant documents
  val generalDocumentMapTermFrequency = MutMap[String, Map[String, Int]]()
  val generalDocumentMapTermFrequency_2 = MutMap[String, Map[String, Int]]() // half document
  val generalDocumentMapTermFrequency_5 = MutMap[String, Map[String, Int]]() // 1/5 of document

  var generalDocumentMapLogTermFrequency = MutMap[String, Map[String, Double]]()
  var generalDocumentMapLogTermFrequency_2 = MutMap[String, Map[String, Double]]() // half document
  var generalDocumentMapLogTermFrequency_5 = MutMap[String, Map[String, Double]]() // 1/5 of document

  def score_basic(query_tokenized: List[String], docName: String, docEuclideanLength: Double) = {

      val tfs: Map[String, Int] = generalDocumentMapTermFrequency.get(docName).get
      val qtfs = query_tokenized.flatMap(q => tfs.get(q))
      val numTermsInCommon = qtfs.length  // Number of query terms inside document

      val queryLen = query_tokenized.length.toDouble
      val termOverlap = qtfs.sum.toDouble / (docEuclideanLength * queryLen)


      (numTermsInCommon.toDouble / queryLen + termOverlap, termOverlap)
  }

  def logtf(tf: Map[String, Int]): Map[String, Double] = {
    val sum = tf.values.sum.toDouble
    tf.mapValues(v => log2((v.toDouble + 1.0) / sum))
  }

  def score_tf_idf(query_tokenized: List[String], doc: String, docName: String, pstemmer: Boolean) = {


    var qterms = query_tokenized.filter(!stopWords.contains(_))

    if (pstemmer) {
      qterms = qterms.map(PorterStemmer.stem(_))
    }

    def tf(docName: String): Map[String, Int] = generalDocumentMapTermFrequency.get(docName).get
    def tf_2(docName: String): Map[String, Int] = generalDocumentMapTermFrequency_2.get(docName).get
    def tf_5(docName: String): Map[String, Int] = generalDocumentMapTermFrequency_5.get(docName).get

    def logtf(docName: String): Map[String, Double] = generalDocumentMapLogTermFrequency.get(docName).get
    def logtf_2(docName: String): Map[String, Double] = generalDocumentMapLogTermFrequency_2.get(docName).get
    def logtf_5(docName: String): Map[String, Double] = generalDocumentMapLogTermFrequency_5.get(docName).get

    var ltf = Map[String, Double]()
    var ltf_2 = Map[String, Double]()
    var ltf_5 = Map[String, Double]()

    qterms.map(q => ltf += q -> logtf(docName).getOrElse(q, 0))
    qterms.map(q => ltf_2 += q -> logtf_2(docName).getOrElse(q, 0))
    qterms.map(q => ltf_5 += q -> logtf_5(docName).getOrElse(q, 0))

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

    (score1, score2, score3, score4)
  }

  def score_title(query_tokenized_porter_stemmer: List[String], docTitle: String, tfs: Map[String, Int]) = {

    val length = query_tokenized_porter_stemmer.length

    val qtfs = query_tokenized_porter_stemmer.flatMap(q => tfs.get(q))

    qtfs.length.toDouble / length // percentage of terms in common

  }

  def get_doc_frequency(docs: Iterator[XMLDocument]) = {

    /** Get document frequency and inverse document frequency. */
    for(currentDocument <- docs) {

      if(documentCounter % 1000 == 0)
        println(documentCounter)

      documentCounter += 1

      val dterms = Tokenizer.tokenize(currentDocument.content.toLowerCase.trim())
      countTotalTermsInAllDocs += dterms.size

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

    generalDocumentMapLogTermFrequency = generalDocumentMapTermFrequency.map(t => t._1 -> logtf(t._2))
    generalDocumentMapLogTermFrequency_2 = generalDocumentMapTermFrequency_2.map(t => t._1 -> logtf(t._2))
    generalDocumentMapLogTermFrequency_5 = generalDocumentMapTermFrequency_5.map(t => t._1 -> logtf(t._2))


    collectionSize = documentCounter
    val logCollectionSize = log2(collectionSize)

    df.foreach(kv => idf += kv._1 -> (logCollectionSize - log2(kv._2)))
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

  class LanguageModelResult(docName: String, score: Double) {
    val correspondingDoc = docName
    val relevancy = score
  }
  implicit def orderedNode(idfResult: LanguageModelResult) = new Ordered[LanguageModelResult] {
    def compare(other: LanguageModelResult) = {
      idfResult.relevancy.compare(other.relevancy)
    }
  }
  def getLanguageModelScore(queryTerms: MutSet[String], docContent: String): Double = {
    /** Find the language model score of given document for given query. */
    var logProbabilityOfQuery = 0.0
    val lambda = 0.5

    val docTokenized = Tokenizer.tokenize(docContent)
    val overlappingTerms = queryTerms.intersect(docTokenized.toSet)

    for(term <- overlappingTerms) {
      val countOfTermInDocument = docTokenized.filter(_ == term).size
      val probabilityOfTermInDocument = countOfTermInDocument / docTokenized.size
      val probabilityOfTermInCollection = cf(term) / countTotalTermsInAllDocs
      val logProbabilityOfTerm =
        log2(1 + (1-lambda)/lambda * probabilityOfTermInDocument.toDouble / probabilityOfTermInCollection) + log2(lambda)

      logProbabilityOfQuery += logProbabilityOfTerm
    }

    return logProbabilityOfQuery
  }

  var best1000FeaturesForRanking = List[mutable.PriorityQueue[FeatureArray]]() // based on tf-idf

  var featureVectorsUsedForTraining = Array[Array[Double]]()
  var labelsForTraining = Array[Int]()

  def second_pass(docs: Iterator[XMLDocument], topics: MutMap[Int, String], scoresCollectionSorted: List[List[String]]) =
  {
    val numOfTopics = 40

    // Initialise top-lists of results (both for language model and for machine learning model)
    for(i <- 0 to numOfTopics - 1)
      {
        // Candidates for machine learning model
        best1000FeaturesForRanking = best1000FeaturesForRanking.+:(new mutable.PriorityQueue[FeatureArray]())

        // Best results from language model
        languageModelResultLists(i + 51) = new mutable.PriorityQueue[LanguageModelResult]()
      }

    documentCounter = 0

    val all_topics_sorted = topics.toList.sortWith(_._1 < _._1)

    var all_queries_tokenized_porter_stemmer = Array[List[String]]()
    var all_queries_tokenized = Array[List[String]]()

    for (topic <- all_topics_sorted)
      {
        val qterms_tokenized = Tokenizer.tokenize(topic._2.toLowerCase)
        val qterms_tokenized_porter_stemmer = qterms_tokenized.map(PorterStemmer.stem(_))

        all_queries_tokenized = all_queries_tokenized.:+(qterms_tokenized)
        all_queries_tokenized_porter_stemmer = all_queries_tokenized_porter_stemmer.:+(qterms_tokenized_porter_stemmer)
      }


    for(doc <- docs)
      {
        if(documentCounter % 1000 == 0)
          println(documentCounter)

        documentCounter += 1

        val doc_content = doc.content.toLowerCase.trim()
        val doc_title = doc.title.toLowerCase.trim()
        val doc_name = doc.name.trim().replace("-","")

        // no porter stemmer
        val tfs_content: Map[String, Int] = Tokenizer.tokenize(doc_content).groupBy(identity).mapValues(l => l.length)
        val doc_euclidean_length = tfs_content.values.map(x => x * x).sum.toDouble

        // PorterStemmer only applied for title of document
        val titleTerms = Tokenizer.tokenize(doc_title).map(PorterStemmer.stem(_))
        val tfs_title: Map[String, Int] = titleTerms.groupBy(identity).mapValues(l => l.length)


        var topic_counter = -1

        for(topic <- all_topics_sorted)
          {
              topic_counter += 1

              val query_title = topic._2

              val score1 = score_basic(all_queries_tokenized(topic_counter), doc_name, doc_euclidean_length)
              //val score1 = (0,0)
              val score2 = score_title(all_queries_tokenized_porter_stemmer(topic_counter), doc_title, tfs_title)

              val score3 = score_tf_idf(all_queries_tokenized(topic_counter), doc_content, doc_name, false)


              val feature_array = Array(score1._1, score1._2, score2, score3._1, score3._2, score3._3, score3._4, -100) // don't care about last number.. just for WEKA Library (relevance is placed in training vectors)
              best1000FeaturesForRanking(topic_counter).enqueue(new FeatureArray(doc.name.trim(), feature_array))

              if (best1000FeaturesForRanking(topic_counter).size == 501) {
                // keep only best 1000 features..
                best1000FeaturesForRanking(topic_counter).dequeue()
              }

              // Language model: update result list
              val languageModelScore = getLanguageModelScore(queryTerms, doc_content) // TODO
              languageModelResultLists(topic._1).enqueue(new LanguageModelResult(doc_name, languageModelScore))
              if(languageModelResultLists(topic._1).size > 100) {
                // Make sure we keep only 100 top results
                languageModelResultLists(topic._1).dequeue()
              }



              //if (current_topic_in_qrel == (topic_counter + 51) && current_doc_name_in_qrel.equals(doc_name)) {
              if (documentQrelMap.contains(doc_name) && documentQrelMap(doc_name).contains(topic_counter + 51)) {

                // current query - document pair is in qrel
                val relevance = documentQrelMap(doc_name)(topic_counter + 51)

                featureVectorsUsedForTraining = Array(score1._1, score1._2, score2, score3._1, score3._2, score3._3, score3._4, relevance) +: featureVectorsUsedForTraining // :+ Array(score1._1, score1._2, score2, score3._1, score3._2, score3._3, score3._4, relevance)
                labelsForTraining = relevance +: labelsForTraining // :+ relevance
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

    // Fill doc-qrel map
    for(rowString <- scoresCollection) {
      val row = rowString.split(" ").toList
      val docID = row(2).replace("-", "")
      val topicID = row(0).toInt
      val relevancy = row(3).toInt

      val docMap = documentQrelMap.getOrElse(docID, MutMap[Int, Int]())
      docMap(topicID) = relevancy
      documentQrelMap(docID) = docMap
    }

    // Create map 'topic number -> topic title'
    topicsCollection.filter(p => p.startsWith("<num>")).foreach(f => topics +=
      topicsCollection(topicsCollection.indexOf(f)).replace(" ", "").takeRight(2).toInt -> topicsCollection(topicsCollection.indexOf(f) + 6)
        .replace("<title>", "").replace("Topic:", "").toLowerCase.trim())


    /****************** FIRST PASS **********/

    println("Started first pass.")

    documentsInTrainingSet = scoresCollection.map(x => (x.split(" ").toList)(2).replace("-", "")).distinct.toSet

    topics.foreach(t => queryTerms ++= Tokenizer.tokenize(t._2).distinct.filter(!StopWords.stopWords.contains(_)))

    println(queryTerms)

    var tipster = new TipsterCorpusIterator(data_dir_path + "allZips")

    get_doc_frequency(tipster)

    println("Frequencies computed...")

    //generalDocumentMapTermFrequency.foreach{ case p => println(); println(p._1); p._2.foreach{case m => println(m._1+ " " + m._2) } }


    println("Processed documents for first time: " + documentCounter)

    /********* FINISHED FIRST PASS **********/


    /************** SECOND PASS *************/

    println("Started second pass.")

    tipster = new TipsterCorpusIterator(data_dir_path + "allZips")

    scoresCollectionSorted = scoresCollection.map(s => s.split(" ").toList.map(e => e.replace("-", ""))).sortWith(_(2) < _(2))

    second_pass(tipster, topics, scoresCollectionSorted)

    /************** SECOND PASS *************/

    return (featureVectorsUsedForTraining, labelsForTraining)
  }

}
