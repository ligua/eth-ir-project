package main.scala

import scala.util.Random
import scala.util.hashing.MurmurHash3
import scala.runtime.Tuple2Zipped
import java.security.MessageDigest



object SimilarityDetector {

  val mainObject = Main_Object
  
  private type Shingle = List[String]
  private type Token = String
  private type Fingerprint = String
  private type PermutationTable = List[String]  // This list contains all the fingerprints of all documents we have
                                                // stored so far in a given permutation.
  private type Permutation = Seq[Int]

  private val maxBitsToCompare = 10       // How many bits we will compare in each permutation
  private val numPermutations = 100       // Number of random permutations of fingerprint bits (k on lecture slides) we will use
  private val wordsPerShingle = 2
  private val nearDuplicateLimit = 0.9    // Minimum similarity between 2 documents to consider them near duplicates
  private val jaccardNearDuplicateLimit = 0.7
  private val collisionLimit = 1          // How many tables should give a collision so we would consider a candidate
  
  private var numOfExactDuplicates = 0
  private var numOfNearDuplicates = 0
  
  // Initialise fingerprints tables and permutations
  private var fingerprintTables = List.fill(numPermutations)(List[String]()) // contains numPermutations of PermutationTable
  private var urlsSaved = List[String]()
  
  private var permutations = List[Permutation]()

  for(i <- 1 to numPermutations) {
    // Create random permutation and remember it
    val perm = Random.shuffle(0 to 31).toList  // Using 0 to 31 because they are indexes of a 32-char string
    permutations = perm :: permutations
  }


  /** Detect if given document is similar to or duplicate of some document we've already seen.
    * This is the function that will be called from inside the crawler */
  def isSimilarOrDuplicate(queryDoc: String, url: String): (Boolean, Boolean) = {

    val shingles = shingle(tokenize(queryDoc), wordsPerShingle)
    //println(shingles)
    val fingerprint = shinglesToFingerprint(shingles)

    /* this will be a list of number of times (out of the numPermutations) our query document had the same top bits
     * for each of the documents we stored so far
     */
    var numOfSimilarity = List.fill(fingerprintTables(0).size)(0);

    var queryDocPermutations = List[Fingerprint]()

    for(i <- 0 to numPermutations-1) {
      // Permute the fingerprint with the i-th permutation
      val perm = permutations(i)
      val reordered = reorderFingerprint(fingerprint, perm)

      // Remember permuted string so we can add it to the right table later
      // take care ":+" appends element to the end of the list
      //           "+:" prepends element to the list (put the element at the head of the list)
      queryDocPermutations = queryDocPermutations.:+(reordered)

      // Find all candidate fingerprints in this table that have same top bits
      val table = fingerprintTables(i)
      val boolTable = hasSameTopBitsInTable(reordered, table)

      numOfSimilarity = (numOfSimilarity.zip(boolTable)).map{case(x,y) => if (y) x+1 else x}
    }

    // Get all candidates (from permutation 0)
    val candidates = (urlsSaved, fingerprintTables(0), numOfSimilarity).zipped.toList
      .filter{case(_, _, count) => count >= collisionLimit}
      .map{case(u, fp, _) => (u,fp)}

    // Get our query string (from permutation 0)
    val reordered0 = queryDocPermutations(0)

    // Calculate similarities with all candidates
    val similarities = candidates.map{ case(u, fp) => (u,hammingSimilarity(fp, reordered0))}
    
    val maxSim = if(similarities.isEmpty) ("",0.0) else similarities.maxBy(_._2)
    
    var isExactDuplicate = (maxSim._2 == 1.0)
    var isNearDuplicate = (maxSim._2 >= nearDuplicateLimit && maxSim._2 < 1.0)
    
    if(isExactDuplicate) 
    {
      println("EXACT: \n" + url + "\n" + maxSim._1 +"\n"+maxSim._2)  
      numOfExactDuplicates = numOfExactDuplicates + 1
    }
    
    if(isNearDuplicate)
    {
      val candidateURLsForJaccard = similarities.filter(_._2 > nearDuplicateLimit).map{x => x._1}
    
      val checkJaccard = getJaccardSimilarity(url, candidateURLsForJaccard)
      
      if(checkJaccard._2 > jaccardNearDuplicateLimit)
      {
        println("NEAR: \n" + url + "\n" + checkJaccard._1+"\n"+checkJaccard._2)
        numOfNearDuplicates = numOfNearDuplicates + 1
      }
    }

    if(!isExactDuplicate) {
      fingerprintTables = (fingerprintTables.zip(queryDocPermutations)).map{case(x,y) => x.:+(y)}
      urlsSaved = urlsSaved.:+(url)
    }
    
    return (isExactDuplicate, isNearDuplicate)
  }

  /** Check if given fingerprint table has anything that matches the given fingerprint in top 'maxBitsToCompare' bits */
  private def hasSameTopBitsInTable(fp: Fingerprint, table: PermutationTable): List[Boolean] = {

    table.map { fpStore  => (fpStore.take(maxBitsToCompare) == fp.take(maxBitsToCompare)) }
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
    doc.toLowerCase.split("[ .,;:?!\t\n\r\f]+").toList
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

  /** Hash a shingle into 128-bit binary string */
  private def hashShingle128(s: Shingle): String = {
    val seed = 42
    val hasher = MessageDigest.getInstance("MD5")
    val bytes = hasher.digest(s.toString.getBytes("UTF-8")) //MurmurHash3.listHash(s, seed).toBinaryString

    bytes.map(x => x.toInt.toBinaryString).mkString("")
    //String.format("%" + 128 + "s", binString).replace(' ', '0')
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

    fingerprint.mkString
    
  }


  /** Calculate the proportion of positions where s1 and s2 have equal characters */
  private def hammingSimilarity(s1: Fingerprint, s2: Fingerprint): Double = {
    s1.zip(s2).map{case (a, b) => if(a == b) 1 else 0}.sum.toDouble / s1.length
  }
  
  /** calculates the jaccard similarity for the candidate urls that had simhash value of more than
   *  "nearDuplicateLimit" with the query url. 
   */
  private def getJaccardSimilarity(url: String, candidates: List[String]): (String, Double) = {
    val dataOfQuery =  mainObject.getTextAndLinksFromUrl(url)
    val textOfQuery = dataOfQuery._1
    
    val shinglesOfQuery = shingle(textOfQuery, wordsPerShingle)
    
    var maxJacc = 0.0;
    var urlWithMaxJacc = ""
    for(i <- 0 to candidates.size - 1)
    {
      val dataOfCandidate =  mainObject.getTextAndLinksFromUrl(candidates(i))
      val textOfCandidate = dataOfCandidate._1
      
      val shinglesOfCandidate = shingle(textOfCandidate, wordsPerShingle)
      val intersectionSet = shinglesOfCandidate & shinglesOfQuery
      val unionSet = shinglesOfCandidate | shinglesOfQuery
      
      val jaccCoeff = intersectionSet.size*1.0 / unionSet.size
      
      if(jaccCoeff > maxJacc)
      {
        maxJacc = jaccCoeff
        urlWithMaxJacc = candidates(i)
      }
    }
    
    (urlWithMaxJacc, maxJacc)
  }
  /** return the total number of near duplicate and exact duplicate urls. To be called from the main function 
   *  after crawling procedure finishes */ 
  def getNumOfExactAndNearDuplicates(): (Int, Int) = 
  {
    (numOfExactDuplicates, numOfNearDuplicates)
  }


}
