
# An agent-based model for informality in job markets


## Running the model

To run the model locally , you need to have `sbt` installed: https://www.scala-sbt.org/download.html

It can be run with the command `sbt run`. The main class code can be modified in `RunABM.scala`.

To run the model with OpenMOLE, you need to compile it as an OpenMOLE extension into an osgiBundle. For this, run the command `sbt osgiBundle`. Then the jar available in `target/scala-2.13` can be uploaded as a plugin into the OpenMOLE GUI (or used as a parameter for the command line using `-p` option). The model API is then available to a `ScalaTask`, for example using the following syntax:

```
val model = ScalaTask("""
 |import org.igp.abmjobmarket._
 |implicit val runtimeParameters: RuntimeParameters = RuntimeParameters(dataDirectory = (workDirectory / "data").toString+"/")
 |val parameters = ModelParameters(
 |  workersNumber = input.workersNumber, jobsNumber = input.jobsNumber, jobSeekingNumber = input.jobSeekingNumber, unemploymentShare = input.unemploymentShare,
 |  workPermitShare = input.workPermitShare, perceivedInformalityCoef = input.perceivedInformalityCoef, jobSimilarityHierarchy = input.jobSimilarityHierarchy,
 |  socialNetworkCoef = input.socialNetworkCoef, socialNetworkHierarchy = input.socialNetworkHierarchy, socialNetworkMode = input.socialNetworkMode, 
 |  iterations = input.iterations, seed = input.replication
 |)
 |val result = ABM.runModel(parameters)
 |val informality = result.stationaryInformality
 |val unemployment = result.stationaryUnemployment
""".stripMargin
) set (
  plugins += pluginsOf[org.igp.abmjobmarket.ModelParameters],
  resources += workDirectory / "data",
  (inputs, outputs) += (workersNumber, jobsNumber, jobSeekingNumber, unemploymentShare, workPermitShare, perceivedInformalityCoef, jobSimilarityHierarchy, socialNetworkCoef, socialNetworkHierarchy, socialNetworkMode, iterations, replication, id),
  outputs += (informality, unemployment),
  workersNumber := 100, jobsNumber := 200, jobSeekingNumber := 20, iterations := 500
)
```



