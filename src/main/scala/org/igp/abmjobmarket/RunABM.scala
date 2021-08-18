package org.igp.abmjobmarket

import scala.util.Random

object RunABM {

  val workerFilePath = ""

  def setup(workersNum: Int)(implicit rng: Random): ModelState = {

    val workers  = Worker.syntheticWorkerPopulationDataSample(workerFilePath, workersNum)

    ModelState(
      workers = workers,
      employers = Seq.empty,
      jobs = Seq.empty
    )

  }

  def modelStep(state: ModelState): ModelState = state

  def runModel(): Unit = {

  }

}
