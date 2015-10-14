package main.scala

import scala.io.Source
import scala.collection.mutable.{Map => MutMap}

object LanguageDetector {

  type Model = Map[String, Double]

  val models = Array("en", "de").map(lang => (lang, readModel(lang))).toMap

  /** Tokenize a document for language detection */
  def tokenize(doc: String): Iterator[String] = {
    doc.toLowerCase().replaceAll("\\s", " ").replaceAll("[^a-z -]", "").sliding(3)
  }

  /** Read in models */
  def readModel(lang: String): Model = {
    Source.fromURL(getClass.getResource(s"/$lang.txt")).getLines()
      .map(line => {
      val split = line.replace("\\s$", "").split("\t") // Split and remove whitespace at end of line
      (split(0), split(1).toDouble)
    }).toMap
  }

  /** Calculate log-likelihood for a given model and document */
  def logLikelihood(model: Model, doc: String): Double = {

    // Calculate ngram frequencies
    val result = MutMap[String,Int]()
    for (ngram <- tokenize(doc)) {
      result(ngram) = result.getOrElse(ngram,0) + 1
    }

    // Calculate log-likelihood
    var llh = 0.0
    var num_missing_llh_values = 0
    result.toMap.foreach(pair => pair match { case (ngram, occurrences) => {
      val ngram_llh_result = model.get(ngram)
      ngram_llh_result match {
        case Some(x) => llh += occurrences * x
        case None => num_missing_llh_values += occurrences
      }
    }})

    // Take into account missing values
    val num_total_ngrams = result.size
    llh = llh * num_total_ngrams / (num_total_ngrams - num_missing_llh_values)

    llh
  }

  /** Detect language of a given document */
  def detect(doc: String) = {
    var bestLang = ""
    var bestLLH = Double.NegativeInfinity

    models.foreach(langModelPair => langModelPair match {
      case (lang, model) => {
        val llh = logLikelihood(model, doc)
        if(llh > bestLLH) {
          bestLang = lang
          bestLLH = llh
        }
      }
    })

    bestLang
  }

}
