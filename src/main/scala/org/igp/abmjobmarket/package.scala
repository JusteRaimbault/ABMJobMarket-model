package org.igp

/**
 *
 */
package object abmjobmarket {

  import Utils._

  val DEBUG: Boolean = true

  /**
   * Model parameters
   * @param workersNumber number of workers
   * @param jobsNumber number of jobs
   * @param unemploymentShare external control for rate of unemployment
   * @param iterations total model iterations
   * @param seed random seed
   */
  case class ModelParameters(
                              workersNumber: Int = 100,
                              jobsNumber: Int = 200,
                              unemploymentShare: Double = 0.25,
                              jobSeekingNumber: Int = 20,
                              workPermitShare: Double = 0.5,
                              perceivedInformalityCoef: Double = 0.0,
                              jobSimilarityHierarchy: Double = 1.0,
                              socialNetworkCoef: Double = 0.0,
                              socialNetworkHierarchy: Double = 1.0,
                              socialNetworkMode: String = "proximity", // \in {"random", "proximity"}
                              iterations: Int = 1000,
                              seed: Long = 0L,
                              discreteChoiceParams: Array[Double] = Array.empty[Double],
                            )

  /*
  val defaultParameters: ModelParameters = ModelParameters(
    seed = 0L,
    iterations = 1000,
    workersNumber = 100,
    jobsNumber = 1000,
    unemploymentShare = 0.25,
    workPermitShare = 0.5,
    discreteChoiceParams = Array.fill(8)(1.0),
    jobSeekingNumber = 15
  )*/

  case class RuntimeParameters(
                              dataDirectory: String
                              )

  /**
   * Model state at one time step
   * @param workers worker
   * @param employers employers (not used in the current implementation)
   * @param jobs jobs
   * @param jobSimilarities matrix of similarities between jobs (note: with static impl (no employers); no need to store all)
   * @param pastUtilities utilities for each worker and each job, the last time they were observed (sparse at the beginning)
   * @param parameters parameters
   */
  case class ModelState(
                       workers: Seq[Worker],
                       employers: Seq[Employer],
                       jobs: Seq[Job],
                       jobSimilarities: Map[Int, Map[Int, Double]],
                       socialNetwork: Map[Int, Map[Int, Double]],
                       pastUtilities: Map[Int, Map[Int, Double]],
                       parameters: ModelParameters
                       )

  /**
   * Result of a model run
   * @param states full model states
   * @param informality informality time serie
   * @param unemployment unemplyment time serie
   */
  case class ModelResult(
                          states: Seq[ModelState],
                          informality: Array[Double],
                          unemployment: Array[Double]
                        ) {

    /**
     * Compare two results:
     * sum of absolute differences between full time series of informality and unemployment
     * @param result2 other result
     * @return
     */
    def delta(result2: ModelResult): Double= {
      informality.zip(result2.informality).map{case(i1,i2) => math.abs(i1-i2)}.sum + unemployment.zip(result2.unemployment).map{case(i1,i2) => math.abs(i1-i2)}.sum
    }

    def stationaryInformality: Double = informality.stationaryAverage
    def stationaryUnemployment: Double = unemployment.stationaryAverage

    override def toString: String = s"Model result: informality = ${stationaryInformality} ; unemployment = ${stationaryUnemployment}"
  }

  object ModelResult {

    def apply(states: Seq[ModelState]): ModelResult = {
      val informality = states.map(Indicators.informality).toArray
      val unemployment = states.map(Indicators.unemployment).toArray
      ModelResult(states, informality, unemployment)
    }

  }

}
