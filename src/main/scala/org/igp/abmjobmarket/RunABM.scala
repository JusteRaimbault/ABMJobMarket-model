package org.igp.abmjobmarket

object RunABM extends App {

  // global runtime parameters - no default value as system/path dependent
  implicit val runtimeParameters: RuntimeParameters = RuntimeParameters(
    dataDirectory = System.getenv("CS_HOME")+"/Misc/BiblioDocs-tmp/UrbanDynamics/ABMJobs/STATA_20210818/DATA/Cleaning/"
  )

  val result = ABM.runModel()

  println("Informality : "+result.informality.toSeq)
  println("Unemployment : "+result.unemployment.toSeq)

}
