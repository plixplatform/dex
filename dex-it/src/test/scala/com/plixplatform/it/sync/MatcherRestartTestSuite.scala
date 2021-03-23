package com.plixplatform.it.sync

import com.plixplatform.common.state.ByteStr
import com.plixplatform.it.MatcherSuiteBase
import com.plixplatform.it.api.OrderBookResponse
import com.plixplatform.it.api.SyncHttpApi._
import com.plixplatform.it.api.SyncMatcherHttpApi._
import com.plixplatform.it.sync.config.MatcherPriceAssetConfig._
import com.plixplatform.it.util._
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.util.Random

class MatcherRestartTestSuite extends MatcherSuiteBase {
  private def orderVersion = (Random.nextInt(2) + 1).toByte

  "check order execution" - {
    "make order and after matcher's restart try to cancel it" in {
      // Alice issues new asset
      val aliceAsset =
        node
          .broadcastIssue(alice,
                          "DisconnectCoin",
                          "Alice's coin for disconnect tests",
                          someAssetAmount,
                          0,
                          reissuable = false,
                          smartIssueFee,
                          None,
                          waitForTx = true)
          .id
      node.waitForHeight(node.height + 1)

      val alicePlixPair = AssetPair(IssuedAsset(ByteStr.decodeBase58(aliceAsset).get), Plix)
      // check assets's balances
      node.assertAssetBalance(alice.address, aliceAsset, someAssetAmount)
      node.assertAssetBalance(matcher.address, aliceAsset, 0)

      // Alice places sell order
      val aliceOrder = node
        .placeOrder(alice, alicePlixPair, OrderType.SELL, 500, 2.plix * Order.PriceConstant, matcherFee, orderVersion)
      aliceOrder.status shouldBe "OrderAccepted"
      val firstOrder = aliceOrder.message.id

      node.waitOrderStatus(alicePlixPair, firstOrder, "Accepted")

      // check that order is correct
      val orders = node.orderBook(alicePlixPair)
      orders.asks.head.amount shouldBe 500
      orders.asks.head.price shouldBe 2.plix * Order.PriceConstant

      // sell order should be in the node orderbook
      node.fullOrderHistory(alice).head.status shouldBe "Accepted"

      // reboot matcher's node
      docker.killAndStartContainer(node)

      node.waitOrderStatus(alicePlixPair, firstOrder, "Accepted")
      node.fullOrderHistory(alice).head.status shouldBe "Accepted"

      val orders1 = node.orderBook(alicePlixPair)
      orders1.asks.head.amount shouldBe 500
      orders1.asks.head.price shouldBe 2.plix * Order.PriceConstant

      val aliceSecondOrder =
        node.placeOrder(alice, alicePlixPair, OrderType.SELL, 500, 2.plix * Order.PriceConstant, matcherFee, orderVersion, 5.minutes)
      aliceSecondOrder.status shouldBe "OrderAccepted"

      val orders2 =
        node.waitFor[OrderBookResponse]("Top ask has 1000 amount")(_.orderBook(alicePlixPair), _.asks.head.amount == 1000, 1.second)
      orders2.asks.head.price shouldBe 2.plix * Order.PriceConstant

      val cancel = node.cancelOrder(alice, alicePlixPair, firstOrder)
      cancel.status should be("OrderCanceled")

      val orders3 = node.orderBook(alicePlixPair)
      orders3.asks.head.amount shouldBe 500

      node.waitOrderStatus(alicePlixPair, firstOrder, "Cancelled")
      node.fullOrderHistory(alice).head.status shouldBe "Accepted"
    }
  }
}
