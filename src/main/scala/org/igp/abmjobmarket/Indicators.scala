package org.igp.abmjobmarket

object Indicators {

  /**
   * share of informal workers
   * @param state model state
   * @return
   */
  def informality(state: ModelState): Double = {
    val employed = state.workers.filter(_.currentJob!=Job.unemployed)
    if (employed.isEmpty) 0.0 else
    employed.count(!_.currentJob.withContract).toDouble / employed.size.toDouble
  }

  /**
   * unemployment
   * @param state model state
   * @return
   */
  def unemployment(state: ModelState): Double =
    if (state.workers.isEmpty) 1.0 else
    state.workers.count(_.currentJob==Job.unemployed).toDouble / state.workers.size.toDouble


}
