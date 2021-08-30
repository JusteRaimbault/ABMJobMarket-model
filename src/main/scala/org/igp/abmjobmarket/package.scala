package org.igp

import scala.util.Random

/**
 *
 */
package object abmjobmarket {

  /**
   * Model parameters
   * @param workersNumber number of workers
   * @param jobsNumber number of jobs
   * @param unemploymentShare external control for rate of unemployment
   * @param iterations total model iterations
   * @param seed random seed
   */
  case class ModelParameters(
                              workersNumber: Int,
                              jobsNumber: Int,
                              unemploymentShare: Double,
                              workPermitShare: Double,
                              discreteChoiceParams: Array[Double],
                              jobSeekingNumber: Int,
                              iterations: Int,
                              seed: Long
                            )

  val defaultParameters: ModelParameters = ModelParameters(
    seed = 0L,
    iterations = 1000,
    workersNumber = 100,
    jobsNumber = 1000,
    unemploymentShare = 0.25,
    workPermitShare = 0.5,
    discreteChoiceParams = Array.fill(7)(1.0),
    jobSeekingNumber = 15
  )

  case class RuntimeParameters(
                              dataDirectory: String
                              )

  case class ModelState(
                       workers: Seq[Worker],
                       employers: Seq[Employer],
                       jobs: Seq[Job],
                       parameters: ModelParameters
                       )

  case class ModelResult(
                          states: Seq[ModelState],
                          informality: Array[Double],
                          unemployment: Array[Double]
                        )

  object ModelResult {

    def apply(states: Seq[ModelState]): ModelResult = {
      val informality = states.map(Indicators.informality).toArray
      val unemployment = states.map(Indicators.unemployment).toArray
      ModelResult(states, informality, unemployment)
    }

  }

}
