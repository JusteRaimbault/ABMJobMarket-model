package org.igp

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
                              workersNumber: Int = 100,
                              jobsNumber: Int = 200,
                              unemploymentShare: Double = 0.25,
                              workPermitShare: Double = 0.5,
                              discreteChoiceParams: Array[Double] = Array.fill(7)(1.0),
                              perceivedInformalityCoef: Double = 1.0,
                              jobSeekingNumber: Int = 20,
                              iterations: Int = 1000,
                              seed: Long = 0L
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

  case class ModelState(
                       workers: Seq[Worker],
                       employers: Seq[Employer],
                       jobs: Seq[Job],
                       jobSimilarities: Array[Array[Double]],
                       parameters: ModelParameters
                       )

  case class ModelResult(
                          states: Seq[ModelState],
                          informality: Array[Double],
                          unemployment: Array[Double]
                        ) {
    def delta(result2: ModelResult): Double= {
      informality.zip(result2.informality).map{case(i1,i2) => math.abs(i1-i2)}.sum + unemployment.zip(result2.unemployment).map{case(i1,i2) => math.abs(i1-i2)}.sum
    }
  }

  object ModelResult {

    def apply(states: Seq[ModelState]): ModelResult = {
      val informality = states.map(Indicators.informality).toArray
      val unemployment = states.map(Indicators.unemployment).toArray
      ModelResult(states, informality, unemployment)
    }

  }

}
