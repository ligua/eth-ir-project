package main.scala

import ch.ethz.dal.tinyir.io.TipsterStream
import main.scala.Main._

object FeatureExtractor {

  def extract(doc: Document): Features = ???
    /** Extract numerical features from a single document. */
    // TODO

  def extract(stream: TipsterStream): Stream[Features] = ???
    /** Extract numerical features from a stream of documents. */
    // TODO

}
