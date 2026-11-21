organization := "com.github.cascala"
name := "Galileo"
version := "0.2.1"
scalaVersion := "2.13.16"
scalacOptions ++= Seq( "-deprecation", "-feature" )
libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.0.8" % "test"
libraryDependencies += "org.jline" % "jline" % "3.13.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"



// Publication to Sonatype Ivy - artefacts
publishMavenStyle := true
//useGpg := false
publishTo := { 
	val nexus = "https://oss.sonatype.org/"
	if ( isSnapshot.value ) 
    	Some( "snapshots" at nexus + "content/repositories/snapshots" ) 
    else
    	Some( "releases"  at nexus + "service/local/staging/deploy/maven2" )
}

Test / publishArtifact := false

pomIncludeRepository := { _ => false }

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

homepage := Some(url("https://github.com/cascala/galileo"))

pomExtra := (
	<scm>
		<url>git@github.com:cascala/galileo.git</url>
		<connection>scm:git:git@github.com:cascala/galileo.git</connection>
  	</scm>
  	<developers>
    	<developer>
      		<id>cascala</id>
      		<name>Cascala</name>
      		<url>https://github.com/cascala</url>
		</developer>
	</developers>)