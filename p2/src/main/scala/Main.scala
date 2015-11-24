package project2

import ch.ethz.dal.tinyir.io.TipsterStream
import ch.ethz.dal.tinyir.processing.{StopWords, Tokenizer, XMLDocument}
import com.github.aztek.porterstemmer.PorterStemmer
import weka.core.{Attribute, FastVector, Instance, Instances}

object Main {

  val df = collection.mutable.Map[String, Int]()
  val idf = collection.mutable.Map[String, Double]()

  val df_stem = collection.mutable.Map[String, Int]()
  val idf_stem = collection.mutable.Map[String, Double]()

  var collectionSize: Int = 20000
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

  def score_basic(query: String, doc: String) = {
    val qterms = Tokenizer.tokenize(query).distinct
    val length = qterms.length

    def score(doc: List[String]): (Double, Double) = {
      val tfs: Map[String, Int] = doc.groupBy(identity).mapValues(l => l.length)
      val qtfs = qterms.flatMap(q => tfs.get(q))
      val numTermsInCommon = qtfs.length
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

  def testStream() = {

    // Load tipster articles
    val tipster = new TipsterStream("data/zips")
    println("Number of files in zips = " + tipster.length)

    //Load dataset of training topics and scores
    val topicsCollection: List[String] = io.Source.fromFile("data/topics").getLines().toList
    val scoresCollection: List[String] = io.Source.fromFile("data/qrels").getLines().toList
    val topics = scala.collection.mutable.Map[Int, String]()

    topicsCollection.filter(p => p.startsWith("<num>")).foreach(f => topics +=
      topicsCollection(topicsCollection.indexOf(f)).replace(" ", "").takeRight(2).toInt -> topicsCollection(topicsCollection.indexOf(f) + 6)
        .replace("<title>", "").replace("Topic:", "").toLowerCase.trim())

    // Load a small portion of the data (for debugging purposes, all collection will be further required)
    val subCollection = scala.collection.mutable.Map[String, XMLDocument]()
    tipster.stream.take(collectionSize).foreach(p => subCollection.put(p.name, p))
    println("Loading done")

    // Compute document and inversed document frequencies
    get_doc_frequency(subCollection);
    println("Frequencies computed")


    // Extract features from first 50 query-document pairs

    var features = Array[Array[Double]]()
    var labels = Array[Int]()

    for (i <- 0 to scoresCollection.size - 1) {

      val item = scoresCollection(i).split(" ").toList

      val queryId = item(0).toInt
      val docId = item(2).replace("-", "")
      val relevance: Int = item(3).toString.toInt

      val query = topics(queryId)

      try {

        val document = subCollection(docId)
        val doc_title = document.title.toLowerCase.trim()
        val doc_content = document.content.toLowerCase.trim()

        val score1 = score_basic(query, doc_content)
        val score2 = score_title(query, doc_title)
        val score3 = score_tf_idf(query, doc_content, false)
        //val score4 = score_tf_idf(query, doc_content, true)

        features = features :+ Array(score1._1, score1._2, score2, score3._1, score3._2, score3._3, score3._4, relevance)
        labels = labels :+ relevance
        println(relevance, score1._1, score1._2, score2, score3._1, score3._2, score3._3, score3._4)

      } catch {
        case e: Exception => null
      }

    }

    // Perform classification using Random Forest from Weka ML library

    var numFeatures = features(0).length
    var train_size = features.length

    var fvVector: FastVector = new FastVector(numFeatures);
    for (i <- 0 until numFeatures - 1) fvVector.addElement(new Attribute(Integer.toString(i)));
    var fvClassVal: FastVector = new FastVector(2);
    fvClassVal.addElement("1");
    fvClassVal.addElement("-1");
    var ClassAttribute: Attribute = new Attribute("TheClass", fvClassVal);
    fvVector.addElement(ClassAttribute);

    var trainingInstances: Instances = new Instances("Name", fvVector, train_size);
    trainingInstances.setClassIndex(numFeatures - 1);

    for (i <- 0 until train_size) {

      val iExample: Instance = new Instance(numFeatures)
      for (j <- 0 until numFeatures - 1) {
        iExample.setValue(fvVector.elementAt(j).asInstanceOf[Attribute], features(i)(j))
      }

      iExample.setValue(ClassAttribute, (labels(i)*2 - 1).toString)
      trainingInstances.add(iExample)
    }

    var forest : weka.classifiers.trees.RandomForest = new weka.classifiers.trees.RandomForest();
    forest.setNumTrees(10)

    try {
      forest.buildClassifier(trainingInstances);
    } catch {
      case e: Exception => e.printStackTrace()
    }

    var dataUnlabeled : Instances = new Instances("TestInstances", fvVector, 0);
    dataUnlabeled.setClassIndex(numFeatures - 1);
    for (i <- 0 until train_size) {
      val target : Instance = new Instance(numFeatures);
      for (j <- 0 until numFeatures - 1) target.setValue(fvVector.elementAt(j).asInstanceOf[Attribute], features(i)(j));
      dataUnlabeled.add(target);

      try {
        var p = forest.distributionForInstance(dataUnlabeled.lastInstance());
        var result = 2 * p(0) / (p(0) + p(1)) - 1
        println(labels(i), result)
      } catch {
        case e: Exception => e.printStackTrace();
      }
    }
  }

  def main(args: Array[String]): Unit = {
    testStream()
  }

}
