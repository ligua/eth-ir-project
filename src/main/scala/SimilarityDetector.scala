package main.scala

import scala.util.Random
import scala.util.hashing.MurmurHash3


object SimilarityDetector {

  type Shingle = List[String]
  type Token = String
  type Fingerprint = String
  //type FingerprintTable = ??? TODO decide how to store fingerprint tables
  type Permutation = Seq[Int]

  val maxBitsToCompare = 16   // How many bits we will compare in each permutation
  val numPermutations = 10    // Number of random permutations of fingerprint bits (k on lecture slides) we will use

  // Initialise fingerprints tables and permutations
  var fingerprintTables = List[FingerprintTable]()
  var permutations = List[Permutation]()
  var i = 0
  for(i <- 1 to numPermutations) {
    // TODO create empty FingerprintTable and add it to fingerprintTables

    // Create random permutation and remember it
    val perm = Random.shuffle(0 to 31).toList  // Using 0 to 31 because they are indexes of a 32-char string
    permutations = perm :: permutations
  }


  /** Detect if given document is similar to or duplicate of some document we've already seen.
    * This is the function that will be called from inside the crawler */
  def isSimilarOrDuplicate(queryDoc: String): (Boolean, Boolean) = {
    val shingles = shingle(tokenize(queryDoc), 4)
    val fingerprint = shinglesToFingerprint(shingles)

    var hasExactDuplicate = false
    var hasNearDuplicate = false
    // For each fingerprint table
    var i = 0
    for(i <- 0 to numPermutations-1) {
      // Permute the fingerprint with the i-th permutation and get the i-th permutation table
      val perm = permutations(i)
      val reordered = reorderFingerprint(fingerprint, perm)
      val table = fingerprintTables(i)

      val (hasExactDuplicateInThisTable, hasNearDuplicateInThisTable) = hasSimilarOrDuplicateInTable(reordered, table)
      hasExactDuplicate = hasExactDuplicate || hasExactDuplicateInThisTable
      hasNearDuplicate = hasNearDuplicate || hasNearDuplicateInThisTable
    }

    if(!hasExactDuplicate) {
      // TODO add (permutation of) this fingerprint to each table
    }

    return (hasExactDuplicate, hasNearDuplicate)
  }

  /** Check if given fingerprint table has anything that matches the given fingerprint a) exactly or b) with similarity >0.9 */
  def hasSimilarOrDuplicateInTable(fp: Fingerprint, table: FingerprintTable): (Boolean, Boolean) = {
    // TODO

    return (false, true)
  }


  /** Reorder given fingerprint according to the given permutation */
  def reorderFingerprint(fp: Fingerprint, perm: Permutation): Fingerprint = ???
  // TODO


  /** Tokenize a document for similarity detection */
  def tokenize(doc: String): List[Token] = ???
    // TODO tokenize document into a list of words


  /** Create q-length shingles from a list of tokens */
  def shingle(tokens: List[Token], q: Int): Set[Shingle] = {
    require(q>=1)
    tokens.sliding(q).toSet
  }


  /** Hash a shingle into binary string */
  def hashShingle(s: Shingle): String = {
    val seed = 42
    MurmurHash3.listHash(s, seed).toBinaryString
  }


  /** Hash a set of shingles into a fingerprint string */
  def shinglesToFingerprint(shingles: Set[Shingle]): String = {
    val hashes = shingles.toList.map(x => hashShingle(x)) // Hash each shingle

    var fingerprint = List.fill(32)(0)  // Create list of 32 zeros

    hashes.foreach(hash => {
      val additions = hash.toCharArray.map(x => x.toInt - '0'.toInt).map(x => 2 * x - 1)
      fingerprint = (fingerprint, additions).zipped.map(_ + _)
    })

    fingerprint = fingerprint.map(x => if(x <= 0) 0 else 1) // convert back to binary

    fingerprint.mkString
  }


}
