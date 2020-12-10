package stackoverflow

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import annotation.tailrec
import scala.util.Try

/** A raw stackoverflow posting, either a question or an answer */
case class Posting(
                    postingType: Int,
                    id: Int,
                    acceptedAnswer: Option[Int],
                    parentId: Option[QID],
                    score: Int,
                    tags: Option[String]
                  ) extends Serializable


/** The main class */
object StackOverflow extends StackOverflow {

  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local[*]").setAppName("StackOverflow")
  @transient lazy val sc: SparkContext = new SparkContext(conf)
  sc.setLogLevel("WARN")
  /** Main function */
  def main(args: Array[String]): Unit = {
    val lines = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw = rawPostings(lines)
    val grouped = groupedPostings(raw)
    val scored = scoredPostings(grouped)
    val vectors = vectorPostings(scored)
    val means = kmeans(sampleVectors(vectors), vectors, debug = true)
    val results = clusterResults(means, vectors)
    printResults(results)
  }
}

/** The parsing and kmeans methods */
class StackOverflow extends StackOverflowInterface with Serializable {

  /** Languages */
  val langs =
    List(
      "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
      "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

  /** K-means parameter: How "far apart" languages should be for the kmeans algorithm? */
  def langSpread = 50000

  assert(langSpread > 0, "If langSpread is zero we can't recover the language from the input data!")

  /** K-means parameter: Number of clusters */
  def kmeansKernels = 45

  /** K-means parameter: Convergence criteria */
  def kmeansEta: Double = 20.0D

  /** K-means parameter: Maximum iterations */
  def kmeansMaxIterations = 120

  // Parsing utilities:
  def stringToOptionInt(str: String): Option[Int] = Try(str.toInt).toOption

  def stringToInt(str: String): Int = stringToOptionInt(str).getOrElse(0)

  def lineToPosting(line: String): Posting = {
    val arr: Array[String] = line.split(",")

    Posting(
      postingType = stringToInt(arr(0)),
      id = stringToInt(arr(1)),
      acceptedAnswer = stringToOptionInt(arr(2)),
      parentId = stringToOptionInt(arr(3)),
      score = stringToInt(arr(4)),
      tags = if (arr.length >= 6) Some(arr(5).intern) else None
    )
  }

  /** Load postings from the given file */
  def rawPostings(lines: RDD[String]): RDD[Posting] = {
    lines.map(lineToPosting)
  }

  /** Group the questions and answers together */
  def groupedPostings(postings: RDD[Posting]): RDD[(QID, Iterable[(Question, Answer)])] = {
    val questions = postings.filter(post => post.postingType == 1).map(question => (question.id, question))
    val answers = postings.filter(post => post.postingType == 2 && post.parentId.nonEmpty).map(answer => (answer.parentId.get, answer))
    questions.join(answers).groupByKey()
  }

  /** Compute the maximum score for each posting */
  def scoredPostings(grouped: RDD[(QID, Iterable[(Question, Answer)])]): RDD[(Question, HighScore)] = {

    def answerHighScore(as: Array[Answer]): HighScore = {
      var highScore = 0
      var i = 0
      while (i < as.length) {
        val score = as(i).score
        if (score > highScore)
          highScore = score
        i += 1
      }
      highScore
    }

    grouped.map(groupedPost => (groupedPost._2.head._1, answerHighScore(groupedPost._2.map(_._2).toArray)))
  }

  /** Compute the vectors for the kmeans */
  def vectorPostings(scored: RDD[(Question, HighScore)]): RDD[(LangIndex, HighScore)] = {
    /** Return optional index of first language that occurs in `tags`. */
    def firstLangInTag(tag: Option[String], ls: List[String]): Option[Int] = {
      if (tag.isEmpty) None
      else if (ls.isEmpty) None
      else if (tag.get == ls.head) Some(0) // index: 0
      else {
        val tmp = firstLangInTag(tag, ls.tail)
        tmp match {
          case None => None
          case Some(i) => Some(i + 1) // index i in ls.tail => index i+1
        }
      }
    }

    scored
      .filter(_._1.tags.nonEmpty)
      .map(vs => (firstLangInTag(vs._1.tags, langs).get * langSpread, vs._2)).cache()
  }

  /** Sample the vectors */
  def sampleVectors(vectors: RDD[(LangIndex, HighScore)]): Array[(Int, Int)] = {

    assert(kmeansKernels % langs.length == 0,
      "kmeansKernels should be a multiple of the number of languages studied.")
    val perLang = kmeansKernels / langs.length

    // http://en.wikipedia.org/wiki/Reservoir_sampling
    def reservoirSampling(lang: Int, iter: Iterator[Int], size: Int): Array[Int] = {
      val res = new Array[Int](size)
      val rnd = new util.Random(lang)

      for (i <- 0 until size) {
        assert(iter.hasNext, s"iterator must have at least $size elements")
        res(i) = iter.next
      }

      var i = size.toLong
      while (iter.hasNext) {
        val elt = iter.next
        val j = math.abs(rnd.nextLong) % i
        if (j < size)
          res(j.toInt) = elt
        i += 1
      }

      res
    }

    val res =
      if (langSpread < 500)
      // sample the space regardless of the language
      vectors.takeSample(withReplacement = false, kmeansKernels, 42)
        else
        // sample the space uniformly from each language partition
        vectors.groupByKey.flatMap({
          case (lang, vectors) => reservoirSampling(lang, vectors.toIterator, perLang).map((lang, _))
        }).collect

    assert(res.length == kmeansKernels, res.length)
    res
  }

  //  Kmeans method:

  /** Main kmeans computation */
  @tailrec
  final def kmeans(means: Array[(Int, Int)],
                   vectors: RDD[(Int, Int)],
                   iter: Int = 1,
                   debug: Boolean = false): Array[(Int, Int)]

  = {
    val newAverages =
      vectors
        .map(v => (findClosest(v, means), v))
        .groupBy(_._1)
        .mapValues(vs => averageVectors(vs.map(_._2)))
        .collect
        .toMap

    // you need to compute newMeans
    val newMeans = means.indices.map(i => newAverages.getOrElse(i, means(i))).toArray

    val distance = euclideanDistance(means, newMeans)

    if (debug) {
      println(
        s"""Iteration: $iter
           |  * current distance: $distance
           |  * desired distance: $kmeansEta
           |  * means:""".stripMargin)
      for (idx <- 0 until kmeansKernels)
        println(f"   ${means(idx).toString}%20s ==> ${newMeans(idx).toString}%20s  " +
          f"  distance: ${euclideanDistance(means(idx), newMeans(idx))}%8.0f")
    }

    if (converged(distance))
      newMeans
    else if (iter < kmeansMaxIterations)
      kmeans(newMeans, vectors, iter + 1, debug)
    else {
      if (debug) {
        println("Reached max iterations!")
      }
      newMeans
    }
  }

  //  Kmeans utilities:

  /** Decide whether the kmeans clustering converged */
  def converged(distance: Double): Boolean = {
    distance < kmeansEta
  }

  def squaredDiff(v1: Int, v2: Int): Double = {
    (v1 - v2).toDouble * (v1 - v2)
  }

  /** Return the euclidean distance between two points */
  def euclideanDistance(v1: (Int, Int), v2: (Int, Int)): Double = {
    squaredDiff(v1._1, v2._1) + squaredDiff(v1._2, v2._2)
  }

  /** Return the euclidean distance between two points */
  def euclideanDistance(a1: Array[(Int, Int)], a2: Array[(Int, Int)]): Double = {
    assert(a1.length == a2.length)

    a1.zip(a2).map(vs => euclideanDistance(vs._1, vs._2)).sum
  }

  /** Return the closest point */
  def findClosest(p: (Int, Int), centers: Array[(Int, Int)]): Int = {
    @tailrec
    def findClosestIndex(p: (Int, Int), centers: Array[(Int, Int)],
                         dist: Double, index: Int, bestIndex: Int): Int = {
      if (centers.isEmpty) {
        bestIndex
      } else {
        val tempDist = euclideanDistance(p, centers.head)
        val (newDist, newIndex) = if (tempDist < dist) (tempDist, index) else (dist, bestIndex)
        findClosestIndex(p, centers.tail, newDist, index + 1, newIndex)
      }
    }

    findClosestIndex(p, centers, Double.PositiveInfinity, 0, 0)
  }

  def sumTuple3(v1: (Double, Double, Double), v2: (Double, Double, Double)): (Double, Double, Double) = {
    (v1._1 + v2._1, v1._2 + v2._2, v1._3 + v2._3)
  }

  /** Average the vectors */
  def averageVectors(ps: Iterable[(Int, Int)]): (Int, Int) = {
    val (comp1, comp2, count) = ps.map(vs => (vs._1.toDouble, vs._2.toDouble, 1D)).reduce(sumTuple3)

    ((comp1 / count).toInt, (comp2 / count).toInt)
  }

  //  Displaying results:
  def clusterResults(means: Array[(Int, Int)], vectors: RDD[(LangIndex, HighScore)]): Array[(String, Double, Int, Int)] = {
    val closest = vectors.map(p => (findClosest(p, means), p))
    val closestGrouped = closest.groupByKey()

    val median = closestGrouped.mapValues { vs =>
      val langIndex = vs.groupBy(_._1).mapValues(_.size).maxBy(_._2)._1 / langSpread
      val langLabel: String = langs(langIndex) // most common language in the cluster
      val clusterSize: Int = vs.size
      val langPercent: Double = 100 * vs.count(_._1 / langSpread == langIndex).toDouble / clusterSize // percent of the questions in the most common language
      val sortedScores = vs.map(_._2).toArray.sorted
      val medianScore: Int = {
        if (clusterSize % 2 == 0)
          (sortedScores(clusterSize / 2 - 1) + sortedScores(clusterSize / 2)) / 2
        else
          sortedScores(clusterSize / 2)
      }

      (langLabel, langPercent, clusterSize, medianScore)
    }

    median.collect().map(_._2).sortBy(_._4)
  }

  def printResults(results: Array[(String, Double, Int, Int)]): Unit = {
    println("Resulting clusters:")
    println("  Score  Dominant language (%percent)  Questions")
    println("================================================")
    for ((lang, percent, size, score) <- results)
      println(f"$score%7d  $lang%-17s ($percent%-5.1f%%)      $size%7d")
  }
}