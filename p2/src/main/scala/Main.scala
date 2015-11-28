package main.scala

object Main {

  type Features = Array[Array[Double]]
  type Labels   = Array[Int]

  def testStream() = {

    val extracted = FeatureExtractor.extract_features()

    val features = extracted._1
    val labels = extracted._2

    // Perform classification using Random Forest from Weka ML library
    Classifier.train(features, labels)

    val predictedLabels = Classifier.predict(features)
    labels.zip(predictedLabels).foreach(kv => println(s"true\t${kv._1}, predicted\t${kv._2}"))

    println(s"F1 score: ${Classifier.eval_f1score(labels, predictedLabels)}")
  }

  def main(args: Array[String]): Unit = {
    testStream()
  }

}
