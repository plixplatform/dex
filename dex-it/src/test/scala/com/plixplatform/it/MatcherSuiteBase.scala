package com.plixplatform.it

import java.util.concurrent.ThreadLocalRandom

import com.typesafe.config.{Config, ConfigFactory}
import com.plixplatform.it.MatcherSuiteBase.baseConfig
import com.plixplatform.it.sync.config.MatcherPriceAssetConfig
import com.plixplatform.it.transactions.NodesFromDocker
import com.plixplatform.it.util._
import org.scalatest._

import scala.concurrent.ExecutionContext

abstract class MatcherSuiteBase
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with BeforeAndAfterAll
    with MatcherNode {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  val smartFee         = 0.004.plix
  val minFee           = 0.001.plix + smartFee
  val issueFee         = 1.plix
  val smartIssueFee    = 1.plix + smartFee
  val leasingFee       = 0.002.plix + smartFee
  val tradeFee         = 0.003.plix
  val smartTradeFee    = tradeFee + smartFee
  val twoSmartTradeFee = tradeFee + 2 * smartFee

  protected override def createDocker: Docker = new Docker(
    imageName = "com.plixplatform/dex-it:latest",
    tag = getClass.getSimpleName,
    suiteConfig = baseConfig(ThreadLocalRandom.current().nextInt(0, Int.MaxValue))
  )

  protected def node = dockerNodes().head

  protected def nodeConfigs: Seq[Config] = MatcherPriceAssetConfig.Configs

}

object MatcherSuiteBase {
  private def baseConfig(seed: Int): Config = Option(System.getenv("KAFKA_SERVER")).fold(ConfigFactory.empty()) { kafkaServer =>
    ConfigFactory.parseString(s"""
         |plix.dex.events-queue {
         |  type = kafka
         |  kafka {
         |    servers = "$kafkaServer"
         |    topic = "dex-$seed"
         |  }
         |}""".stripMargin)
  }
}
