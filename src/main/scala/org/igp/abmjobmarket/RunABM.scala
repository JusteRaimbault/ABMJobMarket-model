package org.igp.abmjobmarket

object RunABM extends App {

  // global runtime parameters - no default value as system/path dependent
  implicit val runtimeParameters: RuntimeParameters = RuntimeParameters(
    //dataDirectory = System.getenv("CS_HOME")+"/Misc/BiblioDocs-tmp/UrbanDynamics/ABMJobs/STATA_20210818/DATA/Cleaning/"
    dataDirectory = System.getenv("CS_HOME")+"/Misc/BiblioDocs/UrbanDynamics/ABMJobs/STATA_20220128/DATA/Cleaning/" // newest data
  )

  //val result = ABM.runModel()
  //println("Informality : "+result.informality.toSeq)
  //println("Unemployment : "+result.unemployment.toSeq)

  //val result1 = ABM.runModel(ModelParameters(perceivedInformalityCoef = -10.0,discreteChoiceParams = Array.fill(7)(0.001)))
  //val result2 = ABM.runModel(ModelParameters(perceivedInformalityCoef = 10.0,discreteChoiceParams = Array.fill(7)(0.001)))
  val result1 = ABM.runModel(ModelParameters(unemploymentShare = 0.1))
  val result2 = ABM.runModel(ModelParameters(unemploymentShare = 0.9))

  println(result1 delta result2)

}
