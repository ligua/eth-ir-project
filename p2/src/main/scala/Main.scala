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

  def runModel() = {

    // Extract features from files (two passes over data)
    val extracted = FeatureExtractor.extract_features()
    val featuresForTraining = extracted._1
    val labelsForTraining = extracted._2

    // Perform classification using Random Forest from Weka ML library
    Classifier.train(featuresForTraining, labelsForTraining)

    var topic_counter = 0

    // Initialise file writers for log/debug purposes
    val writer = new PrintWriter(new File("top_100_term_based.txt" ))
    val writer2 = new PrintWriter(new File("top_100_language_based.txt" ))
    val writer_stats = new PrintWriter(new File("statistics_term_based.txt" ))
    val writer_stats2 = new PrintWriter(new File("statistics_language_based.txt" ))

    var averagePs = List[Double]()
    var averagePs2 = List[Double]()

    // Get ranking results and calculate how good our system is
    for(best1000FeatureForTopic <- FeatureExtractor.best1000FeaturesForRanking)
      {
        /********** Prediction and printing out of the term-based model ***************/
        val tmpArrayOf1000BestFeatures = best1000FeatureForTopic.toArray.map(p => p.featureArray)
        val tmpDocumentNamesOf1000BestFeatures = best1000FeatureForTopic.toArray.map(p => p.correspondingDoc)

        val (_, predictedRelevancyProbability) = Classifier.predict(tmpArrayOf1000BestFeatures)

        tmpDocumentNamesOf1000BestFeatures
          .zip(predictedRelevancyProbability)
          .sortWith(_._2 > _._2)
          .map(d => d._1).zipWithIndex.take(100)
          .foreach(r => writer.println((51 + topic_counter)+" "+(r._2 + 1)+" "+r._1))

        val resultList = tmpDocumentNamesOf1000BestFeatures.zip(predictedRelevancyProbability).sortWith(_._2 > _._2).take(100)
        resultList.foreach(writer.println)

        /***** SCORING *****/
        // Get all relevant documents for this topic (ground truth)
        val groundTruth = FeatureExtractor.scoresCollectionSorted
          .filter(row => row(0).toInt == (51 + topic_counter) && row(3).toInt == 1)   // Take only relevant qrels for this topic
          .map(row => row(2))                                                         // Keep only document ID

        // Machine learning model scoring
        val averageP: Double = Classifier.eval_average_precision(groundTruth, resultList.map(x => x._1))
        val scores = Classifier.eval_precision_recall_f1(groundTruth.toSet, resultList.map(x => x._1).toSet)

        write_stats_to_file(writer_stats, 51+topic_counter, averageP, scores)
        println(s"Topic ${51+topic_counter} precision: ${scores._1}, recall: ${scores._2}, F1: ${scores._3}, average p: $averageP")

        averagePs = averageP +: averagePs
        /******************************************************************************************/



        /***********************************    Language model     ********************************/

        // Language model scoring
        val resultList2 = FeatureExtractor.languageModelResultLists(topic_counter+51).toList.map(lmr => lmr.correspondingDoc)
        println(s"resultlist2 size: ${resultList2.size}")
        val averageP2: Double = Classifier.eval_average_precision(groundTruth, resultList2)
        val scores2 = Classifier.eval_precision_recall_f1(groundTruth.toSet, resultList2.toSet)
        write_stats_to_file(writer_stats2, 51+topic_counter, averageP2, scores2)
        // println(s"Topic ${51+topic_counter} precision: ${scores2._1}, recall: ${scores2._2}, F1: ${scores2._3}, average p: $averageP2")
        averagePs2 = averageP2 +: averagePs2

        // Write results to file
        resultList2.zipWithIndex.foreach(kv => writer2.println(s"${topic_counter+51} ${kv._2} ${kv._1}"))
        /******************************************************************************************/


        topic_counter += 1
      }

    writer.close()
    writer_stats.close()

    writer2.close()
    writer_stats2.close()

    // Calculate MAP (Mean Average Precision) score
    val MAP = averagePs.sum / averagePs.size
    val MAP2 = averagePs2.sum / averagePs2.size
    println(s"\nMAP score for machine learning model: $MAP")
    println(s"\nMAP score for language based model: $MAP2")

  }
  
  def write_stats_to_file(writer: PrintWriter, topicID: Int, averageP: Double, scores: (Double, Double, Double)) = {
    /** Write stats to the given file. */
    writer.println(s"Topic $topicID:")
    writer.println("Average Precision = "+averageP)
    writer.println("Precision = "+scores._1)
    writer.println("Recall = "+scores._2)
    writer.println("F1-score = "+scores._3)
  }

  def main(args: Array[String]): Unit = {
    runModel()
  }

}
