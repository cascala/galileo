organization := "com.github.cascala"
name := "Galileo"
version := "0.1.4-SNAPSHOT"
scalaVersion := "2.13.1"
scalacOptions ++= Seq( "-deprecation", "-feature" )
libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.0.8" % "test"
libraryDependencies += "org.jline" % "jline" % "3.13.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

// Maven
resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"
resolvers += Resolver.url("scalasbt", new URL("https://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases")) (Resolver.ivyStylePatterns)
resolvers += Resolver.url("sbt-assembly", new URL("https://dl.bintray.com/sbt/sbt-plugin-releases")) (Resolver.ivyStylePatterns)

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