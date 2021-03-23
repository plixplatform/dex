package com.plixplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.plixplatform.it.MatcherSuiteBase
import com.plixplatform.it.api.SyncHttpApi.{sync => _, _}
import com.plixplatform.it.api.SyncMatcherHttpApi._
import com.plixplatform.it.sync.config.MatcherPriceAssetConfig._
import com.plixplatform.it.util._
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.util.Random

class DisableProducerTestSuite extends MatcherSuiteBase {
  private def matcherConfig = ConfigFactory.parseString(s"""plix.dex.events-queue {
       |  local.enable-storing  = no
       |  kafka.producer.enable = no
       |}""".stripMargin)

  override protected def nodeConfigs: Seq[Config] = Configs.map(matcherConfig.withFallback)
  private def orderVersion                        = (Random.nextInt(2) + 1).toByte

  "check no events are written to queue" - {
    // Alice issues new asset
    val aliceAsset = node.signedBroadcast(IssueEthTx.json()).id
    node.waitForTransaction(aliceAsset)
    node.waitForHeight(node.height + 1)

    val alicePlixPair = AssetPair(IssuedAsset(IssueEthTx.id()), Plix)
    // check assets's balances
    node.assertAssetBalance(alice.address, aliceAsset, IssueEthTx.quantity)
    node.assertAssetBalance(matcher.address, aliceAsset, 0)

    "place an order and wait some time" in {
      // Alice places sell order
      val order1 =
        node.prepareOrder(alice, alicePlixPair, OrderType.SELL, 500, 2.plix * Order.PriceConstant, matcherFee, orderVersion, 20.days)

      node
        .expectIncorrectOrderPlacement(
          order1,
          expectedStatusCode = 501,
          expectedStatus = "NotImplemented",
          expectedMessage = Some("This feature is disabled, contact with the administrator")
        )

      // Alice places buy order
      val order2 =
        node.prepareOrder(alice, alicePlixPair, OrderType.BUY, 500, 2.plix * Order.PriceConstant, matcherFee, orderVersion, 21.days)

      node
        .expectIncorrectOrderPlacement(
          order2,
          expectedStatusCode = 501,
          expectedStatus = "NotImplemented",
          expectedMessage = Some("This feature is disabled, contact with the administrator")
        )

      Thread.sleep(5000)
      node.getCurrentOffset should be(-1)
      node.getLastOffset should be(-1)

      docker.killAndStartContainer(dockerNodes().head)

      node.getCurrentOffset should be(-1)
      node.getLastOffset should be(-1)
    }
  }
}
