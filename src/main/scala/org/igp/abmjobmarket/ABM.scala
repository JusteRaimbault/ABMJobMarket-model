package org.igp.abmjobmarket

import scala.util.Random

object ABM {

  val workerFileName = "worker_DCE.csv"


  def setup(workersNum: Int)(implicit rng: Random, runtimeParameters: RuntimeParameters): ModelState = {

    val workers  = Worker.syntheticWorkerPopulationDataSample(runtimeParameters.dataDirectory+workerFileName, workersNum)

    ModelState(
      workers = workers,
      employers = Seq.empty,
      jobs = Seq.empty
    )

  }

  def modelStep(state: ModelState): ModelState = state

  def runModel(parameters: ModelParameters)(implicit runtimeParameters: RuntimeParameters): ModelResult = {
    import parameters._
    implicit val rng: Random = new Random(seed)

    val initialState = setup(workersNumber)
    ModelResult(
      Iterator.iterate(initialState)(modelStep).take(iterations + 1).toSeq
    )
  }

  def runModel()(implicit runtimeParameters: RuntimeParameters): ModelResult = runModel(defaultParameters)

}
