package org.igp

import scala.util.Random

/**
 *
 */
package object abmjobmarket {

  case class ModelParameters(
                              workersNumber: Int,
                              iterations: Int,
                              seed: Long
                            )

  val defaultParameters: ModelParameters = ModelParameters(seed = 0L, iterations = 1000, workersNumber = 100)

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
