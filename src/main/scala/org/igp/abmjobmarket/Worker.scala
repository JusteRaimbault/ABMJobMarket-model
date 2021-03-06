package org.igp.abmjobmarket

import scala.util.Random
import Utils._

import scala.collection.mutable


/**
 *  ! when a worker finds a job, we should update its socio-eco characteristics? (and then distance matrix)
 *   -> not in this static version of the model
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
 * @param currentUtilities utilities estimated at the previous step by this worker (all time memory stored in the model state utility matrix); stored by job id to save memory
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
                   currentJob: Job = Job.unemployed,
                   currentUtilities: Map[Int, Double] = Map.empty[Int, Double]
                 ) {

  // characteristics of the job and not of the worker are taken into account
  //def discreteChoiceVariables: Seq[Double] = Array(salary, workingHours, experience, if (socialSecurity) 1.0 else 0.0, if (insurance) 1.0 else 0.0, if (contract) 1.0 else 0.0)

  //def discreteChoiceCoefs: Seq[Double] = Array.fill(7)(1.0)

  def socioEcoCharacteristics: Array[Double] =
    Array(employed.toDouble, salary, workingHours, experience, socialSecurity.toDouble, insurance.toDouble, contract.toDouble, foreigner.toDouble, workPermit.toDouble)

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
  def newJobDiscreteChoice(jobs: Seq[Job], perceivedInformalities: Map[Int,Double], socialPerception: Map[Int, Double])(implicit rng: Random): Worker = {

    val filteredJobsAndVars = jobs.map(j => (j, perceivedInformalities(j.id), socialPerception(j.id)))

    val availableJobs = if (foreigner&&(!workPermit)) filteredJobsAndVars.filter(_._1.contract==0.0) else filteredJobsAndVars

    val utilities = availableJobs.map{
      case (j,informality,social) => (j.discreteChoiceVariables++Array(informality,social)).dot(discreteChoiceCoefs)
    }
    //println(s"avg utility = ${utilities.sum/utilities.size}")
    val utilitiesexp = utilities.map(math.exp)
    val s = utilitiesexp.sum
    val probas = utilitiesexp.map(_ / s)

    // DEBUG: probas diff with opposite coef for perceived informalities
    //val utilitiesexpOpp = jobs.zip(perceivedInformalities).map{case (j,informality) => math.exp((j.discreteChoiceVariables++Array(informality*(-1.0), 0.0)).dot(discreteChoiceCoefs))}
    //val sOpp = utilitiesexpOpp.sum; val probasOpp = utilitiesexpOpp.map(_ / sOpp)
    //println(s"Proba diff sum: ${probas.zip(probasOpp).map{case (p1,p2) => math.abs(p1-p2)}.sum}")
    //println(s"Proba diff max: ${probas.zip(probasOpp).map{case (p1,p2) => math.abs(p1-p2)}.max}")
    //println(s"Proba max: ${probas.max}")

    val chosenJob = Utils.randomDrawProbas(availableJobs, probas)

    //val chosenJobOpp = Utils.randomDrawProbas(jobs, probasOpp)
    //if (chosenJob != chosenJobOpp) println(s"chosen job : $chosenJob ; with opp : $chosenJobOpp")

    this.copy(
      employed = true,
      currentJob = chosenJob._1,
      currentUtilities = availableJobs.zip(utilities).map{case ((j,_,_),u) => (j.id, u)}.toMap
    )
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

    // worker characs - note: these are not used in the discrete choice, but in the behavior for some
    val id = raw.head.toInt
    val employed = raw(1) match {case "Yes" => true; case _ => false}
    val salary = raw(2) match {case s if s.isEmpty => 0.0; case s => s.toDouble}
    val workingHours = raw(3) match {case s if s.isEmpty => 0.0; case s => s.toDouble}
    val experience = raw(4) match {case "Less than 6 months" => rng.between(1.0,6.0); case "6-11 months" => rng.between(6.0, 12.0); case "12-17 months" => rng.between(12.0, 18.0); case "18-23 months" => rng.between(18.0,24.0); case "2 to less than 5 years" => rng.between(24.0, 60.0); case "5 to less than 10 years" => rng.between(60.0, 120.0); case "10 years or more" => rng.between(120.0, 140.0); case _ => 0.0}
    val socialSecurity = raw(5).toInt match {case 4 => false; case _ => true}
    val insurance = raw(6) match {case "Yes" => true; case _ => false}
    val contract = raw(7) match {case "No" => false; case _ => true}

    //val diversity = raw(8) match {} // diversity is a job attribute

    val foreigner = raw(8) match {case "Lebanese" => false; case _ => true}
    val permit = raw(9) match {case "Yes" => true; case _ => false}

    // discrete choice params - can be extended to distributions around baseline fitted dc params
    // fixed from estimated values from the DCE - need jobs average -> do after job init? - or now with params set properly before, as a function of jobs
    val dcparams = modelParameters.discreteChoiceParams //++Array(modelParameters.perceivedInformalityCoef, modelParameters.socialNetworkCoef) // additional coefs set at transfo

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
  def newJobsChoice(workers: Seq[Worker],
                    jobSeekingNumber: Int, jobs: Seq[Job],
                    perceivedInformalities: Map[Int,Double],
                    socialPerception: Map[Int, Map[Int, Double]]
                   )(implicit rng: Random): Seq[Worker] = {
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
        val employed = currentWorker.newJobDiscreteChoice(state._3, perceivedInformalities, socialPerception(currentWorker.id))
        val remainingJobs = state._3.filter(_ != employed.currentJob)
        (remaining, state._2 ++ Seq(employed), remainingJobs)
      }
    }

    val employed = if(seeking.nonEmpty) jobChoice(Iterator.iterate((seeking, Seq.empty[Worker], jobs))(jobChoice).takeWhile(_._1.nonEmpty).toSeq.last)._2 else Seq.empty[Worker]
    //println(s"Newly employed: ${employed.size}")

    notSeeking++employed
  }


  /**
   * Random weighted social network (null model when the mechanism is activated)
   * @param n number of workers
   * @param rng rng
   * @return
   */
  def randomSocialNetwork(workers: Seq[Worker])(implicit rng: Random): Map[Int, Map[Int, Double]] =
    workers.map { w =>
      (w.id,
        workers.map(w2 => (w2.id, rng.nextDouble())).toMap
        )
    }.toMap

  /**
   * Social network based on cosine similarity, with a given hierarchy
   * @param workers workers
   * @param socialNetworkHierarchy network hierarchy
   * @return
   */
  def proximitySocialNetwork(workers: Seq[Worker], socialNetworkHierarchy: Double): Map[Int, Map[Int, Double]] = {
    val mins = workers.toArray.map(_.socioEcoCharacteristics).transpose.map(_.min)
    val maxs = workers.toArray.map(_.socioEcoCharacteristics).transpose.map(_.max)
    val res = workers.map { j1 =>
      val x1 = j1.socioEcoCharacteristics.zip(mins.zip(maxs)).map{case (x,(mi,ma)) => (x - mi)/ (ma - mi)}
      val n1 = x1.norm
      (j1.id,
        workers.map { j2 =>
        val x2 = j2.socioEcoCharacteristics.zip(mins.zip(maxs)).map{case (x,(mi,ma)) => (x - mi)/ (ma - mi)}
        val n2 = x2.norm
        val s = if (n1==0.0&&n2==0.0) 1.0 else {
          if (n1==0.0 || n2==0.0) 0.0 else math.pow(x1.dot(x2) / (n1*n2), socialNetworkHierarchy)
        }
        (j2.id,s)
      }.toMap)
    }
    res.toMap
  }

  /**
   * Utilities of jobs perceived trough neighbors in the social network (kind of opinion model)
   *  do not renormalise utilities, otherwise effect throught the socialNetworkCoefs would change in time
   *
   *  Note: matrices multiplication ~ by hand like this is not efficient
   * @param state model state
   * @return
   */
  def perceivedUtilities(state: ModelState): Map[Int, Map[Int, Double]] = {
    import state._
    val transposedPastUtilities: Map[Int, Map[Int, Double]] = pastUtilities.toSeq.flatMap{case (i,jobUtils) => jobUtils.toSeq.map{case (j,u) => (i,j,u)}}.
      groupBy(_._2).map{case (j,s) => (j, s.map{case (i,_,u) => (i,u)}.toMap)}
    socialNetwork.map{case (i, row) =>
      val stot = row.values.sum
      (i,
        transposedPastUtilities.map{case (j, utilities) =>
        (j,row.map{case (ii, s) => s*utilities(ii)}.sum / stot)
      }
      )
    }
  }


  def initialEmptyUtilities(workers: Seq[Worker], jobs: Seq[Job]): Map[Int, Map[Int, Double]] =
    workers.map{w => (w.id,
      jobs.map{j => (j.id, 0.0)}.toMap
      )
    }.toMap


  /**
   * Given workers which have been on the job market (memorised utilities), update the pastUtilities matrix
   * @param workers workers
   * @param pastUtilities matrix
   * @return
   */
  def updatePastUtilities(workers: Seq[Worker], pastUtilities: Map[Int, Map[Int, Double]]): Map[Int, Map[Int, Double]] = {
    // copy as mutable and update (maybe not most efficient way)
    val m = new mutable.HashMap[(Int,Int), Double]
    pastUtilities.foreach{case (i,row) => row.foreach(e => m.put((i,e._1),e._2))}
    workers.foreach(w => w.currentUtilities.foreach{case (j,u) => m.put((w.id,j), u)})
    m.toSeq.groupBy(_._1._1).map{case (i, s) => (i,s.map{case ((_,j),u) => (j,u)}.toMap)}
  }


}
