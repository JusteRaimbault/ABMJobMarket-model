package org.igp.abmjobmarket

import scala.util.Random

object ABM {

  val workerFileName: String = "worker_DCE.csv"
  val jobFileName: String = "jobs.csv"

  def setup(workersNum: Int, jobsNum: Int)(implicit rng: Random, runtimeParameters: RuntimeParameters): ModelState = {

    val workers  = Worker.syntheticWorkerPopulationDataSample(runtimeParameters.dataDirectory+workerFileName, workersNum)

    val jobs = Job.syntheticJobsWeightedDataSample(runtimeParameters.dataDirectory+jobFileName, jobsNum)

    ModelState(
      workers = workers,
      employers = Seq.empty,
      jobs = jobs
    )

  }

  def modelStep(state: ModelState): ModelState = state

  def runModel(parameters: ModelParameters)(implicit runtimeParameters: RuntimeParameters): ModelResult = {
    import parameters._
    implicit val rng: Random = new Random(seed)

    val initialState = setup(workersNumber, jobsNumber)
    ModelResult(
      Iterator.iterate(initialState)(modelStep).take(iterations + 1).toSeq
    )
  }

  def runModel()(implicit runtimeParameters: RuntimeParameters): ModelResult = runModel(defaultParameters)

}
