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
   * @param unemploymentRate external control for rate of unemployment
   * @param iterations total model iterations
   * @param seed random seed
   */
  case class ModelParameters(
                              workersNumber: Int,
                              jobsNumber: Int,
                              unemploymentRate: Double,
                              iterations: Int,
                              seed: Long
                            )

  val defaultParameters: ModelParameters = ModelParameters(
    seed = 0L,
    iterations = 1000,
    workersNumber = 100,
    jobsNumber = 1000,
    unemploymentRate = 0.25
  )

  case class RuntimeParameters(
                              dataDirectory: String
                              )

  case class ModelState(
                       workers: Seq[Worker],
                       employers: Seq[Employer],
                       jobs: Seq[Job]
                       )

  case class ModelResult(
                          states: Seq[ModelState],
                          informality: Double
                        )

  object ModelResult {

    def apply(states: Seq[ModelState]): ModelResult = ModelResult(states, 0.0)

  }

}
