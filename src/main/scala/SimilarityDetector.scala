package main.scala

import scala.util.Random
import scala.util.hashing.MurmurHash3
import scala.runtime.Tuple2Zipped


object SimilarityDetector {

  private type Shingle = List[String]
  private type Token = String
  private type Fingerprint = String
  private type PermutationTable = List[String]  // This list contains all the fingerprints of all documents we have
                                                // stored so far in a given permutation.   
  private type Permutation = Seq[Int]

  private val maxBitsToCompare = 10   // How many bits we will compare in each permutation
  private val numPermutations = 100    // Number of random permutations of fingerprint bits (k on lecture slides) we will use
  private val wordsPerShingle = 3
  
  // Initialise fingerprints tables and permutations
  private var fingerprintTables = List.fill(numPermutations)(List[String]()) // contains numPermutations of PermutationTable
  private var permutations = List[Permutation]()
  
  for(i <- 1 to numPermutations) {
    // Create random permutation and remember it
    val perm = Random.shuffle(0 to 31).toList  // Using 0 to 31 because they are indexes of a 32-char string
    permutations = perm :: permutations
  }


  /** Detect if given document is similar to or duplicate of some document we've already seen.
    * This is the function that will be called from inside the crawler */
  def isSimilarOrDuplicate(queryDoc: String): (Boolean, Boolean) = {
    val shingles = shingle(tokenize(queryDoc), wordsPerShingle)
    val fingerprint = shinglesToFingerprint(shingles)
    
    println(shingles)
    
    /* this will be a list of number of times (out of the numPermutations) our query document had the same top bits
     * for each of the documents we stored so far
     */
    var numOfSimilarity = List.fill(fingerprintTables(0).size)(0);
    
    var queryDocPermutations = List[Fingerprint]()
    
    for(i <- 0 to numPermutations-1) {
      // Permute the fingerprint with the i-th permutation and get the i-th permutation table
      val perm = permutations(i)
      val reordered = reorderFingerprint(fingerprint, perm).take(maxBitsToCompare)
      
      // take care ":+" appends element to the end of the list
      //           "+:" prepends element to the list (put the element at the head of the list)
      queryDocPermutations = queryDocPermutations.:+(reordered)
      
      val table = fingerprintTables(i)

      val boolTable  = hasSimilarTopBitsInTable(reordered, table)
      
      numOfSimilarity = (numOfSimilarity.zip(boolTable)).map{case(x,y) => if (y) x+1 else x}
    }
    
    numOfSimilarity = numOfSimilarity.map(_*100/numPermutations)
    println(numOfSimilarity)
    
    var maxPercentageSimilarity = 0
    
    if(!numOfSimilarity.isEmpty)
      maxPercentageSimilarity = numOfSimilarity.max
    
    var isExactDuplicate = false;
    var isNearDuplicate = false;
    
    if(maxPercentageSimilarity == 100)
    {
      isExactDuplicate = true;
    }
    
    if(maxPercentageSimilarity > 60)
    {
      isNearDuplicate = true;
    }

    if(!isExactDuplicate) {
      fingerprintTables = (fingerprintTables.zip(queryDocPermutations)).map{case(x,y) => x.:+(y)}
    }
    
    return (isExactDuplicate, isNearDuplicate)
  }

  /** Check if given fingerprint table has anything that matches the given fingerprint a) exactly or b) with similarity >0.9 */
  private def hasSimilarTopBitsInTable(fp: Fingerprint, table: PermutationTable): List[Boolean] = {
    // TODO
    
    val topBitsToCompare = fp
    
    table.map { fpStore  => (fpStore == topBitsToCompare) }
  }

  /** Reorder given fingerprint according to the given permutation */
  private def reorderFingerprint(fp: Fingerprint, perm: Permutation): Fingerprint = 
  {
    var res: Fingerprint = ""
    
    for(i <- 0 to perm.size-1)
    {
      res = res.+(fp.charAt(perm(i)))
    }
    
    res
  }

  /** Tokenize a document for similarity detection */
  private def tokenize(doc: String): List[Token] = {
    doc.split("[ .,;:?!\t\n\r\f]+").toList
  }

  /** Create q-length shingles from a list of tokens */
  private def shingle(tokens: List[Token], q: Int): Set[Shingle] = {
    require(q>=1)
    tokens.sliding(q).toSet
  }


  /** Hash a shingle into binary string */
  private def hashShingle(s: Shingle): String = {
    val seed = 42
    val binString = MurmurHash3.listHash(s, seed).toBinaryString
    
    String.format("%" + 32 + "s", binString).replace(' ', '0')
  }


  /** Hash a set of shingles into a fingerprint string */
  private def shinglesToFingerprint(shingles: Set[Shingle]): String = {
    val hashes = shingles.toList.map(x => hashShingle(x)) // Hash each shingle

    var fingerprint = List.fill(32)(0)  // Create list of 32 zeros

    hashes.foreach(hash => {
      val additions = hash.toCharArray.map(x => x.toInt - '0'.toInt).map(x => 2 * x - 1)
      fingerprint = fingerprint.zip(additions).map{case(x,y) => x+y}
    })

    fingerprint = fingerprint.map(x => if(x <= 0) 0 else 1) // convert back to binary

    println(fingerprint.mkString)
    
    fingerprint.mkString
    
  }


}
