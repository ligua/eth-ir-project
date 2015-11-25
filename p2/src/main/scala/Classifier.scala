package main.scala

import weka.core.{Attribute, FastVector, Instance, Instances}
import main.scala.Main._

object Classifier {

  val forest = new weka.classifiers.trees.RandomForest()

  def train(features: Features, labels: Labels): Unit = {
    /** Train the classifier using features and corresponding labels. */

    val numFeatures = features(0).length
    val train_size = features.length

    // Put features and label into representation that WEKA accepts
    val fvVector: FastVector = new FastVector(numFeatures)
    for (i <- 0 until numFeatures - 1) fvVector.addElement(new Attribute(Integer.toString(i)))
    val fvClassVal: FastVector = new FastVector(2)
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


    // Prediction? TODO put into another function
    val dataUnlabeled : Instances = new Instances("TestInstances", fvVector, 0)
    dataUnlabeled.setClassIndex(numFeatures - 1)
    for (i <- 0 until train_size) {
      val target : Instance = new Instance(numFeatures)
      for (j <- 0 until numFeatures - 1) target.setValue(fvVector.elementAt(j).asInstanceOf[Attribute], features(i)(j))
      dataUnlabeled.add(target)

      try {
        val p = forest.distributionForInstance(dataUnlabeled.lastInstance())
        val result = 2 * p(0) / (p(0) + p(1)) - 1
        println(labels(i), result)
      } catch {
        case e: Exception => e.printStackTrace()
      }
    }

  }

}
