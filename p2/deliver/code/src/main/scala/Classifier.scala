package main.scala

import weka.classifiers.Evaluation
import weka.core.Debug.Random
import weka.core.{Attribute, FastVector, Instance, Instances}
import main.scala.Main._

object Classifier {

  val forest = new weka.classifiers.trees.RandomForest()
  var fvVector: FastVector = null
  var fvClassVal: FastVector = null

  def train(features: Features, labels: Labels): Unit = {
    /** Train the classifier using features and corresponding labels. */

    val numFeatures = features(0).length
    val train_size = features.length

    // Setup header information for WEKA about what features and labels look like
    fvVector = new FastVector(numFeatures)
    for (i <- 0 until numFeatures - 1) fvVector.addElement(new Attribute(Integer.toString(i)))
    fvClassVal = new FastVector(2)
    fvClassVal.addElement("1")
    fvClassVal.addElement("-1")
    val ClassAttribute: Attribute = new Attribute("TheClass", fvClassVal)
    fvVector.addElement(ClassAttribute)

    val trainingInstances: Instances = new Instances("Name", fvVector, train_size)
    trainingInstances.setClassIndex(numFeatures - 1)

    for (i <- 0 until train_size) {

      val iExample: Instance = new Instance(numFeatures)
      for (j <- 0 until numFeatures - 1) {
        iExample.setValue(fvVector.elementAt(j).asInstanceOf[Attribute], features(i)(j))
      }

      iExample.setValue(ClassAttribute, (labels(i)*2 - 1).toString)
      trainingInstances.add(iExample)
    }

    // Train random forest
    forest.setNumTrees(20)
    try {
      forest.buildClassifier(trainingInstances)
    } catch {
      case e: Exception => e.printStackTrace()
    }

    println("Confusion matrix:")
    val eval : Evaluation = new Evaluation(trainingInstances)
    eval.crossValidateModel(forest, trainingInstances, 10, new Random(1))
    eval.confusionMatrix().foreach(_.foreach(println))
    println("Number of correctly classified samples")
    println(eval.correct())
  }

  def predict(features: Features): (Labels, PredictedRelevancyProbability) = {
    /** Predict labels of given feature vectors. */

    val numFeatures = features(0).length
    val dataset_size = features.length

    // Create empty dataset to hold test examples
    val dataUnlabeled : Instances = new Instances("TestInstances", fvVector, 0)
    dataUnlabeled.setClassIndex(numFeatures - 1)

    // Empty list to hold labels
    var predictedLabels = Array[Int]()
    var predictedRelevancyProbability = Array[Double]()

    for (i <- 0 until dataset_size) {
      // Add all features to a row and then add the row to test dataset
      val target : Instance = new Instance(numFeatures)
      for (j <- 0 until numFeatures - 1) target.setValue(fvVector.elementAt(j).asInstanceOf[Attribute], features(i)(j))
      dataUnlabeled.add(target)

      // Predict result
      try {
        val p = forest.distributionForInstance(dataUnlabeled.lastInstance())
        val result = 2 * p(0) / (p(0) + p(1)) - 1
        val resultDecision = if (result >= 0) 1 else 0
        predictedLabels = resultDecision +: predictedLabels
        predictedRelevancyProbability = ( p(0) / (p(0) + p(1)) ) +: predictedRelevancyProbability
      } catch {
        case e: Exception => e.printStackTrace()
      }
    }

    return (predictedLabels.reverse, predictedRelevancyProbability.reverse)
  }

  def eval_precision_recall_f1(groundTruth: Set[String], retrieved: Set[String]): (Double, Double, Double) = {
    /** Calculate precision and recall metrics. */
    val tp = groundTruth.intersect(retrieved).size
    val fp = retrieved.size - tp
    val fn = groundTruth.size - tp

    val precision = tp.toDouble / (tp + fp)
    val recall = tp.toDouble / (tp + fn)
    val f1 = 2 * precision * recall / (precision + recall)

    return (precision, recall, f1)
  }

  def eval_average_precision(groundTruth: Seq[String], retrieved: Seq[String]): Double = {
    /** Calculate average precision for this topic. groundTruth are the true relevant documents, retrieved are documents
      * retrieved by our system, ordered from most relevant to least relevant.
      * See https://en.wikipedia.org/wiki/Information_retrieval#Average_precision. */

    var num_retrieved = 0 // # of retrieved documents
    var num_tp = 0        // # of true positives (relevant & retrieved)

    var runningSum = 0.0       // Running sum of P(k) * rel(k)

    for(retrievedDoc <- retrieved) {
      // Calculate precision at this position in the result list
      num_retrieved += 1

      if(groundTruth.contains(retrievedDoc)) {    // Retrieved doc at this position was relevant
        num_tp += 1
        val currentPrecision = num_tp.toDouble / num_retrieved
        runningSum += currentPrecision
      }

      // val currentPrecision = num_tp.toDouble / num_retrieved
      // println(s"precision@${num_retrieved}: ${currentPrecision}")
    }

    return runningSum / Math.min(100, groundTruth.size)

  }


}
