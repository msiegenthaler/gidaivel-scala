import sbt._

class GidaivelProject(info: ProjectInfo) extends DefaultProject(info) with AutoCompilerPlugins {      
  val continuations = compilerPlugin("org.scala-lang.plugins" % "continuations" % "2.8.0")
  override def compileOptions = CompileOption("-P:continuations:enable") :: CompileOption("-unchecked") :: super.compileOptions.toList
  
  override def packageSrcJar= defaultJarPath("-sources.jar")
  val sourceArtifact = Artifact.sources(artifactID)
  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageSrc)
  def ivyPublishConfiguration = new DefaultPublishConfiguration("local", "release") { 
    override protected def deliveredPathPattern = outputPath / "[artifact].[ext]" 
    override def configurations = Some(List(Configurations.Compile, Configurations.Test)) 
  } 
  lazy val deliverIvy = deliverTask(deliverIvyModule, ivyPublishConfiguration, sbt.UpdateLogging.Quiet) 


  val inventsoftReleases = "Inventsoft Release Repository" at "http://mavenrepo.inventsoft.ch/repo"
  val inventsoftSnapshots = "Inventsoft Snapshot Repository" at "http://mavenrepo.inventsoft.ch/snapshot-repo"
  override def managedStyle = ManagedStyle.Maven
  val publishTo = Resolver.sftp("Inventsoft Publish", "foxtrot.inventsoft.ch", "/inventsoft/dev/mavenrepo/snapshot-repo")
  Credentials(Path.userHome / ".ivy2" / ".credentials", log)
  

  val scalaxbee = "ch.inventsoft" % "scala-xbee_2.8.1" % "1.0.0-SNAPSHOT"
  val scalaxmpp = "ch.inventsoft" % "scala-xmpp_2.8.1" % "1.0.0-SNAPSHOT"
  val liftjson = "net.liftweb" % "lift-json_2.8.1" % "2.2"

  val scalatest = "org.scalatest" % "scalatest" % "1.2" % "test"
  val logbackcore = "ch.qos.logback" % "logback-core" % "0.9.24" % "test"
  val logbackclassic = "ch.qos.logback" % "logback-classic" % "0.9.24" % "test"
}
