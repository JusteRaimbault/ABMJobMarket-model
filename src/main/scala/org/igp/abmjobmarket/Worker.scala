package org.igp.abmjobmarket

import scala.util.Random

case class Worker(
                   employed: Boolean,
                   salary: Double,
                   workingHours: Double,
                   experience: Double,
                   socialSecurity: Boolean,
                   insurance: Boolean,
                   contract: Boolean,
                   foreigner: Boolean,
                   currentJob: Job = Job.unemployed
                 ) {

  def discreteChoiceVariables: Seq[Double] = Array(salary, workingHours, experience, if (socialSecurity) 1.0 else 0.0, if (insurance) 1.0 else 0.0, if (contract) 1.0 else 0.0)

  def discreteChoiceCoefs: Seq[Double] = Array.fill(7)(1.0)

  def newJobChoice(potentialJobs: Seq[Job])(implicit rng: Random): Worker = {
    this.copy(currentJob = potentialJobs(rng.nextInt(potentialJobs.size)))
  }

}

object Worker {

  val workerDataFields =  Seq("works", "income", "hours_worked", "time_job", "social_security_cat", "insurance", "contract", "nation_HH")

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
   *
   * @param raw raw data
   * @return
   */
  def apply(raw: Seq[String])(implicit rng: Random): Worker = {
    val employed = raw.head match {case "Yes" => true; case _ => false}
    val salary = raw(1) match {case s if s.length == 0 => 0.0; case s => s.toDouble}
    val workingHours = raw(2) match {case s if s.length == 0 => 0.0; case s => s.toDouble}
    val experience = raw(3) match {case "Less than 6 months" => rng.between(1.0,6.0); case "6-11 months" => rng.between(6.0, 12.0); case "12-17 months" => rng.between(12.0, 18.0); case "18-23 months" => rng.between(18.0,24.0); case "2 to less than 5 years" => rng.between(24.0, 60.0); case "5 to less than 10 years" => rng.between(60.0, 120.0); case "10 years or more" => rng.between(120.0, 140.0); case _ => 0.0}
    val socialSecurity = raw(4).toInt match {case 4 => false; case _ => true}
    val insurance = raw(5) match {case "Yes" => true; case _ => false}
    val contract = raw(6) match {case "No" => false; case _ => true}
    val foreigner = raw(7) match {case "Lebanese" => false; case _ => true}
    Worker(employed, salary, workingHours, experience, socialSecurity, insurance, contract, foreigner)
  }


  /**
   * Sample the raw data for a basic synthetic population
   *    Note: if sample size >> data size, not optimal approach (but random individuals are sampled though)
   * @param file data file
   * @param size population size
   * @param rng rng
   * @return
   */
  def syntheticWorkerPopulationDataSample(file: String, size: Int)(implicit rng: Random): Seq[Worker] = {
    val rawData = Utils.readCSV(file, workerDataFields)
    (1 to size).map(_ => Worker(rawData(rng.nextInt(rawData.length))))
  }



}
