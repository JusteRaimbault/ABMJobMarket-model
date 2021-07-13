package org.igp.abmjobmarket

case class Job(
                salary: Double,
                workingHours: Double,
                experience: Double,
                socialSecurity: Boolean,
                insurance: Boolean,
                contract: Boolean
              ){

}

object Job {

  val unemployed: Job = Job(0.0,0.0,0.0,socialSecurity=false,insurance=false,contract=false)

}
