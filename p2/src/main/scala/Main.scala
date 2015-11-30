package main.scala

import java.io._

object Main {

  type Features = Array[Array[Double]]
  type Labels   = Array[Int]
  type PredictedRelevancyProbability = Array[Double]

  class FeatureArrayWithRelevancy(docName: String, featArr: Array[Double], relProb: Double)
  {
    val correspondingDoc = docName
    val featureArray = featArr

    val relevancyProbability = relProb
  }

  def testStream() = {

    val extracted = FeatureExtractor.extract_features()

    val featuresForTraining = extracted._1
    val labelsForTraining = extracted._2

    // Perform classification using Random Forest from Weka ML library
    Classifier.train(featuresForTraining, labelsForTraining)



    // get 100 best features for each topic

    val best1000FeaturesForTopics = FeatureExtractor.best1000FeaturesForRanking

    var topic_counter = 0

    val (predictedLabels, predictedRelevancyProbability) = Classifier.predict(featuresForTraining)

    labelsForTraining.zip(predictedLabels).foreach(kv => println(s"true\t${kv._1}, predicted\t${kv._2}"))

    println(s"F1 score: ${Classifier.eval_f1score(labelsForTraining, predictedLabels)}")


    for(best1000FeatureForTopic <- FeatureExtractor.best1000FeaturesForRanking)
      {
        val tmpArrayOf1000BestFeatures = best1000FeatureForTopic.toArray.map(p => p.featureArray)
        val tmpDocumentNamesOf1000BestFeatures = best1000FeatureForTopic.toArray.map(p => p.correspondingDoc)

        val (predictedLabels, predictedRelevancyProbability) = Classifier.predict(tmpArrayOf1000BestFeatures)


        val writer = new PrintWriter(new File("query"+(51 + topic_counter)+"_top_100.txt" ))

        tmpDocumentNamesOf1000BestFeatures.zip(predictedRelevancyProbability).sortWith(_._2 > _._2).take(100).foreach(writer.println)

        writer.close()

        topic_counter += 1
      }
  }

  def main(args: Array[String]): Unit = {
    testStream()
  }

}
