package com.plixplatform.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.plixplatform.common.state.ByteStr
import com.plixplatform.it.MatcherSuiteBase
import com.plixplatform.it.api.SyncHttpApi._
import com.plixplatform.it.api.SyncMatcherHttpApi._
import com.plixplatform.it.sync.config.MatcherPriceAssetConfig._
import com.plixplatform.it.util._
import com.plixplatform.dex.market.MatcherActor
import com.plixplatform.dex.model.MatcherModel.Price
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.util.Random

class TradersTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = super.nodeConfigs.map(TradersTestSuite.matcherSettingsOrderV3Allowed.withFallback)

  private def orderVersion = (Random.nextInt(3) + 1).toByte

  "Verifications of tricky ordering cases" - {
    // Alice issues new asset
    val aliceAsset = node
      .broadcastIssue(alice,
                      "AliceCoin",
                      "AliceCoin for matcher's tests",
                      someAssetAmount,
                      0,
                      reissuable = false,
                      smartIssueFee,
                      None,
                      waitForTx = true)
      .id

    // Wait for balance on Alice's account
    node.assertAssetBalance(alice.address, aliceAsset, someAssetAmount)
    node.assertAssetBalance(matcher.address, aliceAsset, 0)
    node.assertAssetBalance(bob.address, aliceAsset, 0)

    // Bob issues a new asset
    val bobAssetQuantity = 10000
    val bobNewAsset =
      node.broadcastIssue(bob, "BobCoin3", "Bob's asset", bobAssetQuantity, 0, reissuable = false, smartIssueFee, None, waitForTx = true).id

    val bobAssetId   = IssuedAsset(ByteStr.decodeBase58(bobNewAsset).get)
    val aliceAssetId = IssuedAsset(ByteStr.decodeBase58(aliceAsset).get)

    val bobPlixPair = AssetPair(
      amountAsset = bobAssetId,
      priceAsset = Plix
    )

    val twoAssetsPair =
      if (MatcherActor.compare(Some(bobAssetId.id.arr), Some(aliceAssetId.id.arr)) < 0)
        AssetPair(
          amountAsset = aliceAssetId,
          priceAsset = bobAssetId
        )
      else
        AssetPair(
          amountAsset = bobAssetId,
          priceAsset = aliceAssetId
        )

    node.assertAssetBalance(bob.address, bobNewAsset, bobAssetQuantity)

    "AssetPair BOB/PLIX vs BOB/NULL" in {
      val trickyBobPlixPairWB58 = AssetPair(
        amountAsset = bobAssetId,
        priceAsset = IssuedAsset(ByteStr.decodeBase58("PLIX").get)
      )

      trickyBobPlixPairWB58.key shouldBe bobPlixPair.key

      val trickyBobPlixPairWS = AssetPair(
        priceAsset = IssuedAsset(ByteStr("PLIX".getBytes())),
        amountAsset = bobAssetId
      )

      val trickyBobOrderWB58 = node.prepareOrder(bob, trickyBobPlixPairWB58, OrderType.BUY, 1, 10.plix * Order.PriceConstant)
      node.expectIncorrectOrderPlacement(trickyBobOrderWB58, 400, "OrderRejected")

      val trickyBobOrderWS = node.prepareOrder(bob, trickyBobPlixPairWS, OrderType.BUY, 1, 10.plix * Order.PriceConstant)
      node.expectIncorrectOrderPlacement(trickyBobOrderWS, 400, "OrderRejected")

      val correctBobOrder   = node.prepareOrder(bob, bobPlixPair, OrderType.BUY, 1, 10.plix * Order.PriceConstant)
      val correctBobOrderId = node.placeOrder(correctBobOrder).message.id
      node.waitOrderStatus(bobPlixPair, correctBobOrderId, "Accepted")

      val markets = node.tradingMarkets().markets.map(x => s"${x.amountAsset}-${x.priceAsset}").toSet

      withClue("hasTrickyBobPlixPairWB58Market") {
        markets.contains(trickyBobPlixPairWB58.key) shouldBe true
      }

      withClue("hasTrickyBobPlixPairWSMarket") {
        markets.contains(trickyBobPlixPairWS.key) shouldBe false
      }

      withClue("bobPlixPair") {
        markets.contains(bobPlixPair.key) shouldBe true
      }

      node.orderBook(bobPlixPair).bids shouldNot be(empty)
      node.cancelOrder(bob, bobPlixPair, correctBobOrderId)
      node.waitOrderStatus(bobPlixPair, correctBobOrderId, "Cancelled")
    }

    "owner moves assets/plix to another account and order become an invalid" - {
      // Could not work sometimes because of NODE-546
      "order with assets" - {
        "moved assets, insufficient assets" in {
          val oldestOrderId = bobPlacesAssetOrder(4000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(4000, twoAssetsPair, bobNewAsset)

          // 5000 plix are rest
          node.broadcastTransfer(bob, alice.address, 5000, matcherFee, Some(bobNewAsset), None, waitForTx = true).id

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            node.waitOrderStatus(bobPlixPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            node.orderStatus(oldestOrderId, bobPlixPair).status shouldBe "Accepted"
          }

          // Cleanup
          node.cancelOrder(bob, twoAssetsPair, oldestOrderId)
          node.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")
          node.broadcastTransfer(alice, bob.address, 5000, matcherFee, Some(bobNewAsset), None, waitForTx = true).id
        }

        "leased plix, insufficient fee" in {
          val bobBalance    = node.accountBalances(bob.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val leaseAmount = bobBalance - matcherFee - matcherFee
          val leaseId     = node.broadcastLease(bob, alice.address, leaseAmount, matcherFee, waitForTx = true).id

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            node.waitOrderStatus(bobPlixPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            node.orderStatus(oldestOrderId, bobPlixPair).status shouldBe "Accepted"
          }

          // Cleanup
          node.cancelOrder(bob, twoAssetsPair, oldestOrderId)
          node.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          node.broadcastCancelLease(bob, leaseId, matcherFee, waitForTx = true).id
        }

        "moved plix, insufficient fee" in {
          val bobBalance    = node.accountBalances(bob.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val transferAmount = bobBalance - matcherFee - matcherFee
          node.broadcastTransfer(bob, alice.address, transferAmount, matcherFee, None, None, waitForTx = true).id

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            node.waitOrderStatus(bobPlixPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            node.orderStatus(oldestOrderId, bobPlixPair).status shouldBe "Accepted"
          }

          // Cleanup
          node.cancelOrder(bob, twoAssetsPair, oldestOrderId)
          node.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")
          node.broadcastTransfer(alice, bob.address, transferAmount, matcherFee, None, None, waitForTx = true).id
        }
      }

      "order with plix" - {
        "leased plix, insufficient fee for one ExchangeTransaction" in {
          // Amount of plix in order is smaller than fee
          val bobBalance = node.accountBalances(bob.address)._1

          val oldestOrderId = bobPlacesWaveOrder(bobPlixPair, 1, 10.plix * Order.PriceConstant)
          val newestOrderId = bobPlacesWaveOrder(bobPlixPair, 1, 10.plix * Order.PriceConstant)

          //      waitForOrderStatus(node, bobAssetIdRaw, id, "Accepted")
          val leaseAmount = bobBalance - matcherFee - 10.plix - matcherFee
          val leaseId     = node.broadcastLease(bob, alice.address, leaseAmount, matcherFee, waitForTx = true).id

          withClue(s"The newest order '$newestOrderId' is Cancelled") {
            node.waitOrderStatus(bobPlixPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            node.orderStatus(oldestOrderId, bobPlixPair).status shouldBe "Accepted"
          }

          // Cleanup
          node.cancelOrder(bob, bobPlixPair, oldestOrderId)
          node.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          node.broadcastCancelLease(bob, leaseId, matcherFee, waitForTx = true)
        }

        "leased plix, insufficient plix" in {
          val bobBalance = node.accountBalances(bob.address)._1
          val price      = 1.plix
          val order2     = bobPlacesWaveOrder(bobPlixPair, 1, price * Order.PriceConstant)

          val leaseAmount = bobBalance - matcherFee - price / 2
          val leaseId     = node.broadcastLease(bob, alice.address, leaseAmount, matcherFee, waitForTx = true).id

          withClue(s"The order '$order2' was cancelled") {
            node.waitOrderStatus(bobPlixPair, order2, "Cancelled")
          }

          // Cleanup
          node.broadcastCancelLease(bob, leaseId, matcherFee, waitForTx = true)
        }

        "moved plix, insufficient fee" in {
          // Amount of plix in order is smaller than fee
          val bobBalance = node.accountBalances(bob.address)._1
          val price      = matcherFee / 2
          val order3     = bobPlacesWaveOrder(bobPlixPair, 1, price * Order.PriceConstant)

          val transferAmount = bobBalance - matcherFee - price
          node.broadcastTransfer(bob, alice.address, transferAmount, matcherFee, None, None, waitForTx = true).id

          withClue(s"The order '$order3' was cancelled") {
            node.waitOrderStatus(bobPlixPair, order3, "Cancelled")
          }

          // Cleanup
          node.broadcastTransfer(alice, bob.address, transferAmount, matcherFee, None, None, waitForTx = true).id
        }
      }
    }
  }

  def bobPlacesWaveOrder(assetPair: AssetPair, amount: Long, price: Price): String = {
    val bobOrder = node.prepareOrder(bob, assetPair, OrderType.BUY, amount, price)
    val order    = node.placeOrder(bobOrder).message.id
    node.waitOrderStatus(assetPair, order, "Accepted")
    order
  }

  def bobPlacesAssetOrder(bobCoinAmount: Int, twoAssetsPair: AssetPair, assetId: String): String = {
    val decodedAsset = IssuedAsset(ByteStr.decodeBase58(assetId).get)
    val bobOrder = if (twoAssetsPair.amountAsset == decodedAsset) {
      node.prepareOrder(bob, twoAssetsPair, OrderType.SELL, bobCoinAmount, 1 * Order.PriceConstant, matcherFee, orderVersion)
    } else {
      node.prepareOrder(bob, twoAssetsPair, OrderType.BUY, 1, bobCoinAmount * Order.PriceConstant, matcherFee, orderVersion)
    }
    val order = node.placeOrder(bobOrder)
    node.waitOrderStatus(twoAssetsPair, order.message.id, "Accepted")
    order.message.id
  }

}

object TradersTestSuite {
  val matcherSettingsOrderV3Allowed: Config = ConfigFactory.parseString("plix.dex { allowed-order-versions = [1, 2, 3] }")
}
