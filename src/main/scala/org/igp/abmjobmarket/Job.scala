package org.igp.abmjobmarket

import org.igp.abmjobmarket.Utils.ArrayDecorator

import scala.util.Random

case class Job(
                salary: Double,
                diversity: Double,
                workingHours: Double,
                experience: Double,
                socialSecurity: Double,
                insurance: Double,
                contract: Double
              ){

  def withContract: Boolean = if (contract==1.0) true else false

  /**
   * array of characteristics for the DC utility
   * hardcoded for: unemployed.productElementNames.toArray.map(this.getDeclaredField(_)) - would be generic impl
   * @return
   */
  def discreteChoiceVariables: Array[Double] = Array(salary, diversity, workingHours, experience, socialSecurity, insurance, contract)

}

object Job {

  // dce_salary => salary, to avoid comma issues in csv
  // Seq("salary","dce_diversity","dce_hours","dce_skills","dce_social_security","dce_insurance","dce_contract","choice")
  val jobDataFields: Seq[String] = Seq("dce_salary","dce_diversity","dce_hours","dce_skills","dce_social_security","dce_insurance","dce_contract","choice")

  val unemployed: Job = Job(0.0,0.0,0.0,0.0,0.0,0.0,0.0)

  /**
   * Variables: dce_salary (! normalisation: chge dce coef), dce_diversity, dce_hours, dce_skills, dce_social_security, dce_insurance, dce_contract
   *  ; weighted by choice at random drawing
   *
   * @param raw raw data
   * @param rng rng
   * @return
   */
  def apply(raw: Seq[String])(implicit rng: Random): Job = {

    //val salary = raw.head match {case "less than 450,000" => rng.between(100000.0,450000.0); case "450,000 to 999,999" => rng.between(450000.0, 999999.0) ; case "1,000,000 to 1,999,999" => rng.between(1000000.0, 1999999.0) ; case "2,000,000 to 3,500,000" => rng.between(2000000.0, 3500000.0) ; case "More than 3,500,000" => rng.between(3500000.0, 5000000.0); case _ => rng.between(100000.0,3500000.0) }
    val salary = raw.head match {case "0" => rng.between(100000.0,450000.0); case "1" => rng.between(450000.0, 999999.0) ; case "2" => rng.between(1000000.0, 1999999.0) ; case "3" => rng.between(2000000.0, 3500000.0) ; case "4" => rng.between(3500000.0, 5000000.0); case _ => rng.between(100000.0,3500000.0) }

    //val diversity = raw(1) match {case "No diversity in the workplace" => 0.0 ; case "Some diversity in the workplace" => 0.5 ; case "A lot of diversity in the workplace" => 1.0; case _ => 0.0}
    // data on 20220128: coded with 0, 1, 2
    val diversity = raw(1) match {case s => s.toDouble}

    //val workingHours = raw(2) match {case "Part time (less than 7hrs/day)" => 0.5; case "0" => 1.0; case _ => 0.0}
    // working hours: recoded as binary: > 48h
    val workingHours = raw(2) match {case s => s.toDouble}

    //val experience = raw(3) match {case "My skills and experience are required in this job" => 1.0; case _ => 0.0}
    val experience = raw(3) match {case s => s.toDouble}

    //val security = raw(4) match {case "Social Security" => 1.0; case _ => 0.0}
    val security = raw(4) match {case s => s.toDouble}

    //val insurance = raw(5) match {case "Private insurance" => 1.0; case _ => 0.0}
    val insurance = raw(5) match {case s => s.toDouble}

    //val contract = raw(6) match {case "Contract" => 1.0; case _ => 0.0}
    val contract = raw(6) match {case s => s.toDouble}

    // do not normalise here -> when computing DC coefs - use average of jobs at t0
    //Job(salary / 3500000.0, diversity, workingHours, experience, security, insurance, contract)
    Job(salary, diversity, workingHours, experience, security, insurance, contract)
  }

  /**
   * Weighted sample of job data
   *  Rq: optimal way to randomly draw is to draw all probas, sort and progressively advance in both seqs (cumulated probas)
   *  here: fill a larger pool with each job times weight x K
   *
   * @param file file
   * @param size number of jobs
   * @param rng rng
   * @return
   */
  def syntheticJobsWeightedDataSample(
                                       file: String,
                                       size: Int
                                       //weightVariable: String // not needed: weight assumed as last column
                                     )(implicit rng: Random): Seq[Job] = {
    val rawData = Utils.readCSV(file, jobDataFields)
    val weights = rawData.map(_.last.toDouble)
    val jobPool: Seq[Seq[String]] = rawData.zip(weights).flatMap{case (r,w) => Seq.fill((100*w).toInt)(r)}
    (1 to size).map(_ => Job(jobPool(rng.nextInt(jobPool.length))))
  }


  /**
   * Compute mean field perceived informality for each job, given a distance matrix between jobs
   *  (not recomputed at each step when jobs are fixed)
   *  Note: as function to transform similarities may change in time, transfo recomputed - this may be improved in simplest case
   * @param state model state
   * @return
   */
  def perceivedInformalities(state: ModelState, similaritiesTransformation: Array[Array[Double]] => Array[Array[Double]] = {w => w}): Seq[Double] = {
    val informalities = state.jobs.map(_.contract).toArray
    val w = similaritiesTransformation(state.jobSimilarities)
    //println(w.flatten.sum)
    w.map{weights =>
      weights.dot(informalities) / weights.sum
    }
  }

  /**
   * compute cosine similarity matrix between jobs (brute force: O(|jobs|*|jobs|))
   * @param jobs jobs
   * @return
   */
  def similarities(jobs: Seq[Job]): Array[Array[Double]] = {
    val mins = jobs.toArray.map(_.discreteChoiceVariables).transpose.map(_.min)
    val maxs = jobs.toArray.map(_.discreteChoiceVariables).transpose.map(_.max)
    println(mins.toSeq)
    println(maxs.toSeq)
    val res = jobs.toArray.map { j1 =>
      val x1 = j1.discreteChoiceVariables.zip(mins.zip(maxs)).map{case (x,(mi,ma)) => (x - mi)/ (ma - mi)}
      val n1 = x1.norm
      jobs.toArray.map { j2 =>
        val x2 = j2.discreteChoiceVariables.zip(mins.zip(maxs)).map{case (x,(mi,ma)) => (x - mi)/ (ma - mi)}
        val n2 = x2.norm
        if (n1==0.0&&n2==0.0) 1.0 else {
          if (n1==0.0 || n2==0.0) 0.0 else x1.dot(x2) / (n1*n2)
        }
      }
    }
    //println(res.toSeq.map(_.sum))
    println(res(0).toSeq)
    res
  }



}
