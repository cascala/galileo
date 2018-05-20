organization := "com.github.cascala"

name := "Galileo"

version := "0.1.2-SNAPSHOT"

scalaVersion := "2.11.8"

scalacOptions ++= Seq( "-deprecation", "-feature" )

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.scala-lang" % "jline" % "2.9.0-1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

// Publication to Sonatype Ivy - artefacts
// Maven

resolvers += Resolver.url("scalasbt", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases")) (Resolver.ivyStylePatterns)

publishMavenStyle := true

publishTo <<= version { ( v: String ) =>
	val nexus = "https://oss.sonatype.org/"
	if ( v.trim.endsWith( "SNAPSHOT" ) ) 
    	Some( "snapshots" at nexus + "content/repositories/snapshots" ) 
    else
    	Some( "releases"  at nexus + "service/local/staging/deploy/maven2" )
}

publishArtifact in Test := false

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