package main.scala

object Tokenizer {
  type Token = String

  def tokenize(doc: String): Iterator[Token] = {
    doc.toLowerCase().replaceAll("\\s", " ").replaceAll("[^a-z -]", "").sliding(3)
  }

}
