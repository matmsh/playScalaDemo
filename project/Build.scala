import sbt._
import Keys._
import play.Project._
import com.github.play2war.plugin._

object ApplicationBuild extends Build {

  val appName         = "playScalaDemo"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    "com.github.play2war.ext" %% "redirect-playlogger" % "1.0.1",
    "jfreechart" % "jfreechart" % "1.0.0"
      
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
       Play2WarPlugin.play2WarSettings: _* ).settings(
        
        Play2WarKeys.servletVersion := "3.0",
       // Add your own project settings here   
        

       // copy source files to target directory.    
       unmanagedResourceDirectories in Compile  <+=  baseDirectory { dir =>  dir/"app/"}
  )

}
