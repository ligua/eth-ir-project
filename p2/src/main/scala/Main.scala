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

    var averagePs = List[Double]()

    for(best1000FeatureForTopic <- FeatureExtractor.best1000FeaturesForRanking)
      {
        val tmpArrayOf1000BestFeatures = best1000FeatureForTopic.toArray.map(p => p.featureArray)
        val tmpDocumentNamesOf1000BestFeatures = best1000FeatureForTopic.toArray.map(p => p.correspondingDoc)

        val (predictedLabels, predictedRelevancyProbability) = Classifier.predict(tmpArrayOf1000BestFeatures)

        val writer = new PrintWriter(new File("query"+(51 + topic_counter)+"_top_100.txt" ))

        val resultList = tmpDocumentNamesOf1000BestFeatures.zip(predictedRelevancyProbability).sortWith(_._2 > _._2).take(100)
        resultList.foreach(writer.println)

        /***** SCORING *****/
        // Get all relevant documents for this topic (ground truth)
        val groundTruth = FeatureExtractor.scoresCollectionSorted
          .filter(row => row(0).toInt == (51 + topic_counter) && row(3).toInt == 1)   // Take only relevant qrels for this topic
          .map(row => row(2))                                                         // Keep only document ID

        // Calculate Average Precision score and keep it
        val averageP = Classifier.eval_average_precision(groundTruth, resultList.map(x => x._1))
        // println(s"Average precision for topic ${51 + topic_counter}: ${averageP}")
        averagePs = averageP +: averagePs

        writer.close()

        topic_counter += 1
      }

    // Calculate MAP (Mean Average Precision) score
    val MAP = averagePs.sum / averagePs.size
    println(s"\nMAP score: $MAP")

  }

  def main(args: Array[String]): Unit = {
    testStream()
  }

}
