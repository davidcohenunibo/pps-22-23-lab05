package u05lab.ex2

import u05lab.ex2.ConferenceReviewing.{ACCEPTED_RELEVANCE, ACCEPTED_SCORE, isAccepted}

enum Question:
  case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question,Int]): Unit
  def loadReview(article: Int, relevance: Int, significant: Int, confidence: Int,fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles: Set[Int]
  def sortedAcceptedArticles: List[(Int, Double)]
  def averageWeightedFinalScoreMap: Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = new ConferenceReviewingImpl()
  def ACCEPTED_SCORE: Int = 5
  def ACCEPTED_RELEVANCE: Int = 8
  def isAccepted(scores: Map[Question, Int]): Boolean =
    scores.get(Question.FINAL).exists(_ >= ACCEPTED_SCORE) && scores.get(Question.RELEVANCE).exists(_ >= ACCEPTED_RELEVANCE)

  val defVal: List[Nothing] = List()
  def review(relevance: Int, significant: Int, confidence: Int, fin: Int): Map[Question, Int] =
    Map(Question.RELEVANCE -> relevance,
      Question.SIGNIFICANCE -> significant,
      Question.CONFIDENCE -> confidence,
      Question.FINAL -> fin)
class ConferenceReviewingImpl extends ConferenceReviewing:
  import Question.*
  import ConferenceReviewing.{review, defVal}
  private var reviews: Map[Int, List[Map[Question, Int]]] = Map.empty
  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, finalScore: Int): Unit =
    this.reviews += article ->
      (reviews.getOrElse(article, defVal) :+ review(relevance, significance, confidence, finalScore))
  override def loadReview(article: Int, map: Map[Question, Int]): Unit =
    this.reviews += article -> (this.reviews.getOrElse(article, defVal) :+ map)
  override def orderedScores(article: Int, question: Question): List[Int] =
    this.reviews.getOrElse(article, defVal).flatMap(_.get(question)).sorted
  override def averageFinalScore(article: Int): Double =
    this.reviews.getOrElse(article, defVal).flatMap(_.get(FINAL)).sum.toDouble / this.reviews(article).size
  override def acceptedArticles: Set[Int] =
    this.reviews.collect { case (article, l) if l.exists(isAccepted) => article }.toSet
  override def sortedAcceptedArticles: List[(Int, Double)] =
    this.acceptedArticles.toList.map(article => (article, this.averageFinalScore(article))).sortBy(_._2)
  override def averageWeightedFinalScoreMap: Map[Int, Double] =
    this.reviews.map { case (article, _) => (article, this.averageFinalScore(article)) }