package org.igp.abmjobmarket

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

}

object Job {

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
    val salary = raw.head match { case "less than 450,000" => rng.between(100000.0,450000.0); case "450,000 to 999,999" => rng.between(450000.0, 999999.0) ; case "1,000,000 to 1,999,999" => rng.between(1000000.0, 1999999.0) ; case "2,000,000 to 3,500,000" => rng.between(2000000.0, 3500000.0) ; case "More than 3,500,000" => rng.between(3500000.0, 5000000.0); case _ => rng.between(100000.0,3500000.0) }
    val diversity = raw(1) match {case "No diversity in the workplace" => 0.0 ; case "Some diversity in the workplace" => 0.5 ; case "A lot of diversity in the workplace" => 1.0; case _ => 0.0}
    val workingHours = raw(2) match {case "Part time (less than 7hrs/day)" => 0.5; case "0" => 1.0; case _ => 0.0}
    val experience = raw(3) match {case "My skills and experience are required in this job" => 1.0; case _ => 0.0}
    val security = raw(4) match {case "Social Security" => 1.0; case _ => 0.0}
    val insurance = raw(5) match {case "Private insurance" => 1.0; case _ => 0.0}
    val contract = raw(6) match {case "Contract" => 1.0; case _ => 0.0}
    Job(salary / 3500000.0, diversity, workingHours, experience, security, insurance, contract)
  }



}
