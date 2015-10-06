package main.scala

object Tokenizer {
  type Token = String

  /** Tokenize a document */
  def tokenize(doc: String): Iterator[Token] = {
    doc.toLowerCase().replaceAll("\\s", " ").replaceAll("[^a-z -]", "").sliding(3)
  }

}
