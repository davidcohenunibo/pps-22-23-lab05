package u05lab.ex2

import org.junit.{Before, Test}
import org.junit.Assert.*

class ConferenceReviewingTest:

  val cr: ConferenceReviewing = ConferenceReviewing()

  @Before
  def init(): Unit =
    //carico una revisione per l'articolo 1
    // - 8 per relevance, significance e final
    // - 7 per confidence
    // si ricordi che l'ordine delle domande è: relevance, significance, confidence, final
    this.cr.loadReview(1, 8, 8, 6, 8) // 4.8 è il voto finale pesato (usato da averageWeightedFinalScoreMap)
    cr.loadReview(1, 9, 9, 6, 9) // 5.4
    cr.loadReview(2, 9, 9, 10, 9) // 9.0
    cr.loadReview(2, 4, 6, 10, 6) // 6.0
    cr.loadReview(3, 3, 3, 3, 3) // 0.9
    cr.loadReview(3, 4, 4, 4, 4) // 1.6
    cr.loadReview(4, 6, 6, 6, 6) // 3.6
    cr.loadReview(4, 7, 7, 8, 7) // 5.6
    val map: Map[Question, Int] = Map(
      Question.RELEVANCE -> 8,
      Question.SIGNIFICANCE -> 8,
      Question.CONFIDENCE -> 7,
      Question.FINAL -> 8
    )

    this.cr.loadReview(4, map)
    this.cr.loadReview(5, 6, 6, 6, 10) // 6.0
    this.cr.loadReview(5, 7, 7, 7, 10) // 7.0

  @Test
  def testOrderedScores(): Unit =
    //l'articolo 2 ha preso su RELEVANCE i due voti 4,9
    assertEquals(List(4, 9), this.cr.orderedScores(2, Question.RELEVANCE))
    //e simile per gli altri
    assertEquals(List(6, 7, 8), this.cr.orderedScores(4, Question.CONFIDENCE))
    assertEquals(List(10, 10), this.cr.orderedScores(5, Question.FINAL))

  @Test
  def testAverageFinalScore(): Unit =
    //l'articolo 1 ha preso voto medio su FINAL pari a 8.5, con scarto massimo 0.01
    //lo scarto massimo è 0.01 perché il voto finale è calcolato come media pesata
    //e quindi non è esattamente 8.5
    //quindi lo scarto massimo è 0.01 perché il voto finale è calcolato come media pesata
    //e quindi non è esattamente 8.5
    assertEquals(8.5, 0.01, this.cr.averageFinalScore(1))
    //e simile per gli altri
    assertEquals(7.5, 0.01, this.cr.averageFinalScore(2))
    assertEquals(3.5, 0.01, this.cr.averageFinalScore(3))
    assertEquals(7.0, 0.01, this.cr.averageFinalScore(4))
    assertEquals(10.0, 0.01, this.cr.averageFinalScore(5))

  @Test
  def testAcceptedArticles(): Unit =
  // solo gli articoli 1,2,4 vanno accettati, avendo media finale >=5 e almeno un voto su RELEVANCE >= 8
    assertEquals(Set(1, 2, 4), this.cr.acceptedArticles)

  @Test
  def testSortedAcceptedArticles(): Unit =
  // articoli accettati, e loro voto finale medio
    assertEquals(List((4, 7.0), (2, 7.5), (1, 8.5)), this.cr.sortedAcceptedArticles)

  @Test
  def optionalTestAverageWeightedFinalScore(): Unit =
    // l'articolo 1 ha media pesata finale pari a (4.8+5.4)/2 = 5,1, con scarto massimo 0.01
    assertEquals((4.8 + 5.4) / 2, 0.01, this.cr.averageWeightedFinalScoreMap(1))
    // e simile per gli altri
    assertEquals((9.0 + 6.0) / 2, 0.01, this.cr.averageWeightedFinalScoreMap(2))
    assertEquals((0.9 + 1.6) / 2, 0.01, this.cr.averageWeightedFinalScoreMap(3))
    assertEquals((3.6 + 5.6 + 5.6) / 3, 0.01, this.cr.averageWeightedFinalScoreMap(4))
    assertEquals((6.0 + 7.0) / 2, 0.01, this.cr.averageWeightedFinalScoreMap(5))
    assertEquals(5, this.cr.averageWeightedFinalScoreMap.size)

