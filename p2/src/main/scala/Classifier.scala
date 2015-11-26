package main.scala

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
    forest.setNumTrees(10)
    try {
      forest.buildClassifier(trainingInstances)
    } catch {
      case e: Exception => e.printStackTrace()
    }

  }

  def predict(features: Features): Labels = {
    /** Predict labels of given feature vectors. */

    val numFeatures = features(0).length
    val dataset_size = features.length

    // Create empty dataset to hold test examples
    val dataUnlabeled : Instances = new Instances("TestInstances", fvVector, 0)
    dataUnlabeled.setClassIndex(numFeatures - 1)

    // Empty list to hold labels
    var predictedLabels = Array[Int]()

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
      } catch {
        case e: Exception => e.printStackTrace()
      }
    }

    return predictedLabels.reverse
  }

}
