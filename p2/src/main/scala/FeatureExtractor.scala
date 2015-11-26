package main.scala

import _root_.ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}
import _root_.com.github.aztek.porterstemmer.PorterStemmer
import main.scala.Main._
import scala.collection.mutable.{Map => MutMap}

object FeatureExtractor {

  def extract_features(//docCollection: MutMap[String, XMLDocument],
                       docCollection: Stream[XMLDocument], // stream of documents relevant for training sorted by title
                       scoresCollection: List[String],
                       topics: MutMap[Int, String]): (Features, Labels) = {
    /** Extract the features for each document in docCollection. */

    var features = Array[Array[Double]]()
    var labels = Array[Int]()

    val scoresCollectionSorted = scoresCollection.map(s => s.split(" ").toList.map(e => e.replace("-", ""))).sortWith(_(2) < _(2))

    val documentIterator = docCollection.iterator

    var currentDocument = documentIterator.next()

    // For each training data point

    var i = 0

    var stopTraining = false

    while (i <= scoresCollectionSorted.size - 1) {

      //val item = scoresCollection(i).split(" ").toList

      val item = scoresCollectionSorted(i)

      val queryId = item(0).toInt
      val docId = item(2).replace("-", "")
      val relevance: Int = item(3).toString.toInt

      if(!documentIterator.hasNext)
      {
        // all documents in subcollection and present in qrels have been used, so stop training
        stopTraining = true
      }

      if(!stopTraining) {
        if (docId == currentDocument.name) {
          // this mean that current qrel is with a document in our stream, so use this training point, otherwise skip

          val query = topics(queryId) // Query is topic title corresponding to given topic ID

          try {
            // Catch error if document of current training data row is not in our subcollection

            val document = currentDocument // TODO (was docId)
            val doc_title = document.title.toLowerCase.trim()
            val doc_content = document.content.toLowerCase.trim()

            val score1 = FeatureExtractor.score_basic(query, doc_content)
            val score2 = FeatureExtractor.score_title(query, doc_title)
            val score3 = FeatureExtractor.score_tf_idf(query, doc_content, false)
            //val score4 = FeatureExtractor.score_tf_idf(query, doc_content, true)

            features = features :+ Array(score1._1, score1._2, score2, score3._1, score3._2, score3._3, score3._4, relevance)
            labels = labels :+ relevance

            println("query #: " + i)
            //println(relevance, score1._1, score1._2, score2, score3._1, score3._2, score3._3, score3._4)

            i = i + 1

          } catch {
            case e: Exception => null
          }
        }
        else {
          if (docId > currentDocument.name) {
            // meaning that current document has been used in all its corresponding training points, so go to next document
            currentDocument = documentIterator.next()
          }
          else {
            // this mean current training data has document id less than the smallest document id in out stream, so this training point will not be relevant, skip it.
            println("skip")
          }
        }
      }
      else
      {
        // to break from loop
        i = scoresCollectionSorted.size
      }
    }

    println("finished")

    return (features, labels)
  }

  def score_basic(query: String, doc: String) = {
    /** Number of terms in  */
    val qterms = Tokenizer.tokenize(query).distinct

    def score(doc: List[String]): (Double, Double) = {
      val tfs: Map[String, Int] = doc.groupBy(identity).mapValues(l => l.length)
      val qtfs = qterms.flatMap(q => tfs.get(q))
      val numTermsInCommon = qtfs.length  // Number of query terms inside document
      val docLen = tfs.values.map(x => x * x).sum.toDouble
      val queryLen = qterms.length.toDouble
      val termOverlap = qtfs.sum.toDouble / (docLen * queryLen)

      (numTermsInCommon.toDouble / queryLen + termOverlap, termOverlap)
    }

    score(Tokenizer.tokenize(doc))
  }

  def score_tf_idf(query: String, doc: String, pstemmer: Boolean) = {

    var qterms = Tokenizer.tokenize(query).distinct.filter(!stopWords.contains(_))
    var dterms = Tokenizer.tokenize(doc)


    if (pstemmer) {
      qterms = qterms.map(PorterStemmer.stem(_))
      dterms = dterms.map(PorterStemmer.stem(_))
    }

    def tf(doc: List[String]): Map[String, Int] = doc.groupBy(identity).mapValues(l => l.length)

    def logtf(tf: Map[String, Int]): Map[String, Double] = {
      val sum = tf.values.sum.toDouble
      tf.mapValues(v => log2((v.toDouble + 1.0) / sum))
    }

    var ltf = Map[String, Double]()
    var ltf_2 = Map[String, Double]()
    var ltf_5 = Map[String, Double]()

    qterms.map(q => ltf += q -> logtf(tf(dterms)).getOrElse(q, 0))
    qterms.map(q => ltf_2 += q -> logtf(tf(dterms.take(dterms.size / 2))).getOrElse(q, 0))
    qterms.map(q => ltf_5 += q -> logtf(tf(dterms.take(dterms.size / 5))).getOrElse(q, 0))

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

  def score_title(query: String, docTitle: String) = {

    val qterms = Tokenizer.tokenize(query).distinct.map(PorterStemmer.stem(_))
    val length = qterms.length

    val titleTerms = Tokenizer.tokenize(docTitle).map(PorterStemmer.stem(_))
    val tfs: Map[String, Int] = titleTerms.groupBy(identity).mapValues(l => l.length)
    val qtfs = qterms.flatMap(q => tfs.get(q))

    qtfs.length.toDouble / length // percentage of terms in common

  }

}
