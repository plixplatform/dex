package com.plixplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.plixplatform.it.MatcherSuiteBase
import com.plixplatform.it.NodeConfigs.Default
import com.plixplatform.it.api.SyncHttpApi._
import com.plixplatform.it.api.SyncMatcherHttpApi._
import com.plixplatform.it.sync.config.MatcherPriceAssetConfig._
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.assets.exchange.{AssetPair, OrderV1}

import scala.concurrent.duration.DurationInt

class BroadcastUntilConfirmedTestSuite extends MatcherSuiteBase {
  private def minerConfig = ConfigFactory.parseString(
    """plix {
      |  network.node-name = node02
      |  extensions = []
      |}""".stripMargin).withFallback(Default.head)

  private def matcherConfig =
    ConfigFactory
      .parseString(s"""plix {
                      |  miner.enable = no
                      |  matcher.exchange-transaction-broadcast {
                      |    broadcast-until-confirmed = yes
                      |    interval = 20s
                      |  }
                      |}""".stripMargin)
      .withFallback(Default.head)

  override protected def nodeConfigs: Seq[Config] = Seq(matcherConfig, minerConfig)

  private def minerDockerNode = dockerNodes().last

  "BroadcastUntilConfirmed" in {
    markup("Issue an asset")
    node.signedBroadcast(IssueEthTx.json())
    val pair = AssetPair(IssuedAsset(IssueEthTx.id()), Plix)
    nodes.waitForTransaction(IssueEthTx.id().toString)
    nodes.waitForHeightArise()

    markup("Prepare orders")
    val now = System.currentTimeMillis()
    val alicePlace = OrderV1.sell(
      sender = alice,
      matcher = matcher,
      pair = pair,
      amount = 100000L,
      price = 80000L,
      timestamp = now,
      expiration = now + 1.day.toMillis,
      matcherFee = 300000L
    )

    val bobPlace = OrderV1.buy(
      sender = bob,
      matcher = matcher,
      pair = pair,
      amount = 200000L,
      price = 100000L,
      timestamp = now,
      expiration = now + 1.day.toMillis,
      matcherFee = 300000L
    )

    markup("Shutdown miners")
    val minerContainerId = docker.stopContainer(minerDockerNode)

    markup("Place orders, those should match")
    node.placeOrder(alicePlace)
    node.placeOrder(bobPlace)
    node.waitOrderStatus(pair, alicePlace.idStr(), "Filled")
    val exchangeTxId = node.waitTransactionsByOrder(alicePlace.idStr(), 1).head.id

    markup("Start miners and wait until it receives the transaction")
    docker.startContainer(minerContainerId)
    nodes.waitForTransaction(exchangeTxId)
  }
}
