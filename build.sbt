scalaVersion := "2.13.1"

name := "abmjobmarket"

version := "0.1-SNAPSHOT"

//mainClass in (Compile, run) := Some("org.igp.abmjobmarket.RunABM")

// model as openmole plugin
enablePlugins(SbtOsgi)
OsgiKeys.exportPackage := Seq("org.igp.abmjobmarket.*")
OsgiKeys.importPackage := Seq("*;resolution:=optional")
OsgiKeys.privatePackage := Seq("!scala.*,*")
OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""

//libraryDependencies ++= Seq(
//  "org.apache.commons" % "commons-math3" % "3.6.1",
//  "com.github.tototoshi" %% "scala-csv" % "1.3.6"
//)
