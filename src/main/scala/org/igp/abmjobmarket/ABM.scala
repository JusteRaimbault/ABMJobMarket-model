package org.igp.abmjobmarket

import scala.util.Random

object ABM {

  val workerFileName: String = "worker_DCE.csv"
  val jobFileName: String = "jobs.csv"

  def setup(parameters: ModelParameters)(implicit rng: Random, runtimeParameters: RuntimeParameters): ModelState = {
    import parameters._

    val workers  = Worker.syntheticWorkerPopulationDataSample(runtimeParameters.dataDirectory+workerFileName, parameters)
    val jobs = Job.syntheticJobsWeightedDataSample(runtimeParameters.dataDirectory+jobFileName, jobsNumber)

    ModelState(
      workers = workers,
      employers = Seq.empty, // no need for employers in the demand-driven only model
      jobs = jobs,
      jobSimilarities = Job.similarities(jobs),
      parameters = parameters
    )

  }

  /**
   * Model step:
   *  - administrative task for some workers (work permit e.g.)
   *  - open jobs update: either employers open new jobs, or a fixed share of workers become unemployed (simple mode, independently of being employed)
   *  - choice by workers of new jobs (fixed share smaller than unemployment)
   * @param state model state
   * @return
   */
  def modelStep(state: ModelState)(implicit rng: Random): ModelState = {
    //println("\n======Model step=====")

    import state.parameters._

    // proportion without work permit for admin
    val foreign = state.workers.filter(_.foreigner)
    val allowedWorkPermitsShare = math.max(0.0,workPermitShare - foreign.count(_.workPermit).toDouble / foreign.size.toDouble)
    val adminFilled = state.workers.map(_.fillAdminTasks(allowedWorkPermitsShare))

    // open jobs through unemployment
    val workers = adminFilled.map(_.faceUnemployment(unemploymentShare))
    //println(s"newly unemployed: ${workers.count(_.currentJob==Job.unemployed) - adminFilled.count(_.currentJob==Job.unemployed)}")

    // choice of new jobs for some unemployed
    // val newlyEmployed = workers.map(_.newJobChoice(state.jobs)) // must be a function of the state, to avoid conflict on the same job
    // perceived informality as current mean field before the current step
    //val perceivedInformality = Indicators.informality(state) // ! depends on each job
    val perceivedInformalities = Job.perceivedInformalities(state)
    //println(s"Perceived informalities = $perceivedInformalities")
    val newlyEmployed = Worker.newJobsChoice(workers, jobSeekingNumber, state.jobs, perceivedInformalities)

    // only workers are updated for now
    state.copy(workers = newlyEmployed)
  }

  /**
   * Run the model
   * @param parameters parameters
   * @param runtimeParameters runtime parameters
   * @return
   */
  def runModel(parameters: ModelParameters)(implicit runtimeParameters: RuntimeParameters): ModelResult = {
    import parameters._
    implicit val rng: Random = new Random(seed)

    val initialState = setup(parameters)
    ModelResult(
      Iterator.iterate(initialState)(modelStep).take(iterations + 1).toSeq
    )
  }

  /**
   * run model with default parameter values
   * @param runtimeParameters implicit runtime parameters
   * @return
   */
  def runModel()(implicit runtimeParameters: RuntimeParameters): ModelResult = //runModel(defaultParameters)
    runModel(ModelParameters())
}
