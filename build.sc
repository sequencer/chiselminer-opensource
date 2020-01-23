import mill._
import scalalib._
import $file.chiselblocks.build

trait CommonModule extends ScalaModule {
  def scalaVersion = "2.12.10"
  def scalacOptions = Seq("-Xsource:2.11")
  def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.2.2",
    ivy"edu.berkeley.cs::chisel-iotesters:1.3.2"
  )
}
object chiselminer extends CommonModule {

  def moduleDeps = Seq(config, asyncqueue, chiselblocks.build.chiselblocks)

  object tests extends Tests {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.0.4")

    def testFrameworks = Seq("org.scalatest.tools.Framework")
  }
}

object config extends ScalaModule {
  def scalaVersion = "2.12.10"

  def millSourcePath = super.millSourcePath / 'design / 'craft
}

object asyncqueue extends CommonModule with SbtModule
