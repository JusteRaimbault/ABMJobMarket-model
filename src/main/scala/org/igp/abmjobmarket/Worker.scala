package org.igp.abmjobmarket

import scala.util.Random
import Utils._


/**
 *
 * @param employed currently employed
 * @param salary salary
 * @param workingHours hourus
 * @param experience experience
 * @param socialSecurity social security
 * @param insurance insurance
 * @param contract contract
 * @param foreigner foreigner
 * @param workPermit no work permit: no contract only
 * @param discreteChoiceCoefs DC fitted coefs; includes belief about unformality
 * @param currentJob current job
 */
case class Worker(
                   id: Int,
                   employed: Boolean,
                   salary: Double,
                   workingHours: Double,
                   experience: Double,
                   socialSecurity: Boolean,
                   insurance: Boolean,
                   contract: Boolean,
                   foreigner: Boolean,
                   workPermit: Boolean,
                   discreteChoiceCoefs: Array[Double],
                   currentJob: Job = Job.unemployed
                 ) {

  // characteristics of the job and not of the worker are taken into account
  //def discreteChoiceVariables: Seq[Double] = Array(salary, workingHours, experience, if (socialSecurity) 1.0 else 0.0, if (insurance) 1.0 else 0.0, if (contract) 1.0 else 0.0)

  //def discreteChoiceCoefs: Seq[Double] = Array.fill(7)(1.0)

  /**
   * uniform random job
   * @param jobs jobs
   * @param rng rng
   * @return
   */
  def newUniformJobChoice(jobs: Seq[Job])(implicit rng: Random): Worker = {
    this.copy(currentJob = jobs(rng.nextInt(jobs.size)))
  }

  /**
   * Choice with discrete choice utility
   *  ! perceived informality must depend on each job, otherwise just add a vanishing contsant in the DC proba
   *  ! work permit is not taken into account
   *
   *
   * @param jobs jobs
   * @param rng rng
   * @return
   */
  def newJobDiscreteChoice(jobs: Seq[Job], perceivedInformalities: Seq[Double])(implicit rng: Random): Worker = {
    val utilitiesexp = jobs.zip(perceivedInformalities).map{case (j,informality) => math.exp((j.discreteChoiceVariables++Array(informality)).dot(discreteChoiceCoefs))}
    val s = utilitiesexp.sum
    val probas = utilitiesexp.map(_ / s)
    val chosenJob = Utils.randomDrawProbas(jobs, probas)
    this.copy(employed = true, currentJob = chosenJob)
  }

  /**
   * only work permit for foreigners is independent of job choice for now
   *  -> random application given external parameter
   *  ! work permit fees may play a role in employers choice (take into account in a more complicated model)
   * @return
   */
  def fillAdminTasks(workPermitShare: Double)(implicit rng: Random): Worker = {
    if (foreigner&&(!workPermit)) {
      if (rng.nextDouble()<workPermitShare) this.copy(workPermit=true) else this
    } else this
  }

  /**
   * can randomly become unemployed
   * @param unemploymentShare share of unemployment
   * @return
   */
  def faceUnemployment(unemploymentShare: Double)(implicit rng: Random): Worker = if (rng.nextDouble()<unemploymentShare) this.copy(currentJob = Job.unemployed, employed = false) else this

}

object Worker {

  val workerDataFields =  Seq("works", "income", "hours_worked", "time_job", "social_security_cat", "insurance", "contract", "nation_HH", "permit")

  // generic data transformation?
  //def workerDataTranform(field: String)

  /**
   *
   * Variable names:
   * works {No, Yes, empty}
   * income (value, empty)
   * hours_worked (value, empty)
   * time_job {Less than 6 months, 6-11 months, 12-17 months, 18-23 months, 2 to less than 5 years, 5 to less than 10 years, 10 years or more}
   * social_security_cat : 4 for No, other for Yes
   * insurance: {Yes, No}
   * contract {Yes, No, Oral, empty}
   * permit {Yes, No, Not applicable, .}
   *
   * @param raw raw data
   * @return
   */
  def apply(raw: Seq[String], modelParameters: ModelParameters)(implicit rng: Random): Worker = {

    // worker characs
    val id = raw.head.toInt
    val employed = raw(1) match {case "Yes" => true; case _ => false}
    val salary = raw(2) match {case s if s.length == 0 => 0.0; case s => s.toDouble}
    val workingHours = raw(3) match {case s if s.length == 0 => 0.0; case s => s.toDouble}
    val experience = raw(4) match {case "Less than 6 months" => rng.between(1.0,6.0); case "6-11 months" => rng.between(6.0, 12.0); case "12-17 months" => rng.between(12.0, 18.0); case "18-23 months" => rng.between(18.0,24.0); case "2 to less than 5 years" => rng.between(24.0, 60.0); case "5 to less than 10 years" => rng.between(60.0, 120.0); case "10 years or more" => rng.between(120.0, 140.0); case _ => 0.0}
    val socialSecurity = raw(5).toInt match {case 4 => false; case _ => true}
    val insurance = raw(6) match {case "Yes" => true; case _ => false}
    val contract = raw(7) match {case "No" => false; case _ => true}
    val foreigner = raw(8) match {case "Lebanese" => false; case _ => true}
    val permit = raw(9) match {case "Yes" => true; case _ => false}

    // discrete choice params - can be extended to distributions around baseline fitted dc params
    val dcparams = modelParameters.discreteChoiceParams++Array(modelParameters.perceivedInformalityCoef)

    Worker(id, employed, salary, workingHours, experience, socialSecurity, insurance, contract, foreigner, permit, dcparams)
  }


  /**
   * Sample the raw data for a basic synthetic population
   *    Note: if sample size >> data size, not optimal approach (but random individuals are sampled though)
   *     -> plus workers must be unique: use without replacement instead
   *
   * @param file data file
   * @param modelParameters params
   * @param rng rng
   * @return
   */
  def syntheticWorkerPopulationDataSample(file: String, modelParameters: ModelParameters)(implicit rng: Random): Seq[Worker] = {
    val rawData = Utils.readCSV(file, workerDataFields, withId = true)
    //(1 to modelParameters.workersNumber).map(_ => Worker(rawData(rng.nextInt(rawData.length)), modelParameters))
    val n = math.min(modelParameters.workersNumber, rawData.size)
    rng.shuffle(rawData).take(n).map(Worker(_, modelParameters))
  }


  /**
   * new job choice for a set of workers
   * @param workers workers
   * @param jobSeekingNumber max number of job seeking workers per time step
   * @param jobs job pool
   * @param perceivedInformalities perceived informality
   * @param rng rng
   * @return
   */
  def newJobsChoice(workers: Seq[Worker], jobSeekingNumber: Int, jobs: Seq[Job], perceivedInformalities: Seq[Double])(implicit rng: Random): Seq[Worker] = {
    val n = math.min(jobSeekingNumber, workers.count(_.currentJob==Job.unemployed))
    val potentialSeeking = workers.filter(_.currentJob==Job.unemployed)
    //val seeking: Seq[Worker] = (1 to n).map(_ => potentialSeeking(rng.nextInt(potentialSeeking.size))).groupBy(w => w).keys.toSeq
    val seeking = rng.shuffle(potentialSeeking).take(n) // use shuffle for random draw without replacement
    val notSeeking = workers.filter(!seeking.contains(_))
    //println(s"New job choice ; workers # = ${workers.size} ; unemployed # = ${potentialSeeking.size} ; seeking # = ${seeking.size} ; not seeking # = ${notSeeking.size}")
    //println(s"Unique workers: ${workers.groupBy(w => w).keys.size}")

    def jobChoice(state: (Seq[Worker],Seq[Worker],Seq[Job])): (Seq[Worker],Seq[Worker],Seq[Job]) = {
      //println(state._1)
      if (state._1.isEmpty) state else {
        val currentWorker = state._1.head
        val remaining = state._1.tail
        val employed = currentWorker.newJobDiscreteChoice(state._3, perceivedInformalities)
        val remainingJobs = state._3.filter(_ != employed.currentJob)
        (remaining, state._2 ++ Seq(employed), remainingJobs)
      }
    }

    val employed = if(seeking.nonEmpty) jobChoice(Iterator.iterate((seeking, Seq.empty[Worker], jobs))(jobChoice).takeWhile(_._1.nonEmpty).toSeq.last)._2 else Seq.empty[Worker]
    //println(s"Newly employed: ${employed.size}")

    notSeeking++employed
  }




}
