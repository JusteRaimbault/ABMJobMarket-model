package org.igp.abmjobmarket

import scala.util.Random

object ABM {

  val workerFileName: String = "worker_DCE.csv"
  val jobFileName: String = "jobs.csv"

  /**
   * Hardcoded empirical DC coefs from DCE
   */
  val empiricalDCCoefs: Map[String, Array[((Double, Double), Double)]] = Map(
    "salary" -> Array(
      ((450000,999999),0.102),
      ((1000000,1999999),0.426),
      ((2000000,3500000),0.658),
      ((3500000, Double.PositiveInfinity),0.666)
    ),
    "workingHours" -> discreteDCCoef(0.122),
    "experience" -> discreteDCCoef(-0.339),
    "socialSecurity" -> discreteDCCoef(0.317),
    "diversity" -> Array(
      ((0.5,1.5),-0.085),
      ((1.5,2.5),-0.129)
    ),
    "insurance" -> discreteDCCoef(0.137),
    "contract" -> discreteDCCoef(-0.258)
  )

  def discreteDCCoef(beta: Double): Array[((Double, Double), Double)] = Array(((Double.NaN, Double.NaN),beta))

  /**
   * Transformation from discrete DC coefs to continous variable
   * @param rawCoefs raw coefs from empirical DCE
   * @param jobs jobs
   * @return coefs array, in order of Job field names (! productElementNames is ordered: ok)
   */
  def transformDCCoefs(rawCoefs: Map[String, Array[((Double, Double), Double)]], jobs: Seq[Job]): Array[Double] = {
    val fieldNames = Job.unemployed.productElementNames.toArray
    val res = fieldNames.map{field =>
      if (rawCoefs(field).head._1._1.isNaN) rawCoefs(field).head._2
      else {
        val rawvals = rawCoefs(field).map(_._2)
        val bounds = rawCoefs(field).map(_._1)
        val fieldValues = jobs.map { j =>
          val getter = j.getClass.getDeclaredMethod(field)
          getter.invoke(j).asInstanceOf[Double]
        }
        val numJobs = fieldValues.size.toDouble
        val avgField = fieldValues.sum / numJobs
        bounds.map{case (min,max) => fieldValues.count(v => v>=min&&v<max).toDouble / (numJobs * avgField)}.zip(rawvals).map{
          case (w, beta) => w*beta
        }.sum
      }
    }
    //Utils.log(res.mkString(" "))
    res
  }

  def setup(parameters: ModelParameters)(implicit rng: Random, runtimeParameters: RuntimeParameters): ModelState = {
    import parameters._

    // setup jobs before workers - dc params adjusted with empirical averages
    val jobs = Job.syntheticJobsWeightedDataSample(runtimeParameters.dataDirectory+jobFileName, jobsNumber)

    // update dc coefs only if empty default is provided (to deactivate: provide full 0.0 array)
    val updatedDCCoefs = if (parameters.discreteChoiceParams.isEmpty) parameters.copy(
      discreteChoiceParams = transformDCCoefs(empiricalDCCoefs, jobs)++Array(parameters.perceivedInformalityCoef, parameters.socialNetworkCoef)
    )
    else parameters

    Utils.log(s"Updated DC params: ${updatedDCCoefs.discreteChoiceParams.toSeq}")

    val workers  = Worker.syntheticWorkerPopulationDataSample(runtimeParameters.dataDirectory+workerFileName, updatedDCCoefs)

    ModelState(
      workers = workers,
      employers = Seq.empty, // no need for employers in the demand-driven only model
      jobs = jobs,
      jobSimilarities = Job.similarities(jobs),
      socialNetwork = Array.empty[Array[Double]],
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
    val perceivedInformalities = Job.perceivedInformalities(
      state,
      similaritiesTransformation = {w => w.map{_.map{s =>
        //math.pow((1.0 + s)/2.0, jobSimilarityHierarchy)
        math.pow(s, jobSimilarityHierarchy)
      }}}
    )
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

    val startTime = System.currentTimeMillis()

    import parameters._
    implicit val rng: Random = new Random(seed)

    val initialState = setup(parameters)
    val res = ModelResult(
      Iterator.iterate(initialState)(modelStep).take(iterations + 1).toSeq
    )

    Utils.log(s"Model run took ${System.currentTimeMillis()-startTime} ms")
    res
  }

  /**
   * run model with default parameter values
   * @param runtimeParameters implicit runtime parameters
   * @return
   */
  def runModel()(implicit runtimeParameters: RuntimeParameters): ModelResult = //runModel(defaultParameters)
    runModel(ModelParameters())
}
