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

  // test perceived informality
  //val result1 = ABM.runModel(ModelParameters(perceivedInformalityCoef = -10.0,discreteChoiceParams = Array.fill(7)(0.001)))
  //val result2 = ABM.runModel(ModelParameters(perceivedInformalityCoef = 10.0,discreteChoiceParams = Array.fill(7)(0.001)))

  // test unemp share
  //val result1 = ABM.runModel(ModelParameters(unemploymentShare = 0.1))
  //val result2 = ABM.runModel(ModelParameters(unemploymentShare = 0.9))

  // test jobSimilarityHierarchy - note: same seed, no random noise - Pb!: same res!
  // ! when all jobs tend to be equiprobable, model is much slower (random drawing)
  //val result1 = ABM.runModel(ModelParameters(jobSimilarityHierarchy = 10.0, perceivedInformalityCoef= -100.0))
  //val result2 = ABM.runModel(ModelParameters(jobSimilarityHierarchy = 10.0, perceivedInformalityCoef=100.0))

  // test social network proximity - Q: complexity social nw? ok
  val result1 = ABM.runModel(ModelParameters(socialNetworkCoef= 10.0, socialNetworkHierarchy = 5.0))
  val result2 = ABM.runModel(ModelParameters(socialNetworkCoef= -10.0, socialNetworkHierarchy = 5.0))

  println(result1 delta result2)
  println(result1)
  println(result2)

}
