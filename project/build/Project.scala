
import sbt._

class SpecsProject(info: ProjectInfo) extends DefaultProject(info) {

  val specsRepo = "Specs Repository" at "http://scala-tools.org/repo-snapshots/"
  val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5-SNAPSHOT"

//  val scalacheck = "org.scala-tools.testing" % "scalacheck" % "1.5"
//  val mockito = "org.mockito" % "mockito-core" % "1.7"
//  val junit = "junit" % "junit" % "4.4"
}
