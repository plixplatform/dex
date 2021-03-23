package com.plixplatform.it.sync

import com.plixplatform.it.MatcherSuiteBase
import com.plixplatform.it.api.SyncHttpApi._
import com.plixplatform.it.api.SyncMatcherHttpApi._
import com.plixplatform.it.sync.config.MatcherPriceAssetConfig._
import com.plixplatform.it.util._
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.assets.exchange.{AssetPair, OrderType}

import scala.concurrent.duration._

class CancelOrderTestSuite extends MatcherSuiteBase {
  private val plixBtcPair = AssetPair(Plix, IssuedAsset(BtcId))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    val xs = Seq(IssueUsdTx, IssueBtcTx).map(_.json()).map(node.signedBroadcast(_))
    xs.foreach(tx => node.waitForTransaction(tx.id))
  }

  "Order can be canceled" - {
    "by sender" in {
      val orderId = node.placeOrder(bob, plixUsdPair, OrderType.SELL, 100.plix, 800, matcherFee).message.id
      node.waitOrderStatus(plixUsdPair, orderId, "Accepted", 1.minute)

      node.cancelOrder(bob, plixUsdPair, orderId)
      node.waitOrderStatus(plixUsdPair, orderId, "Cancelled", 1.minute)

      node.orderHistoryByPair(bob, plixUsdPair).collectFirst {
        case o if o.id == orderId => o.status shouldEqual "Cancelled"
      }
    }
    "with API key" in {
      val orderId = node.placeOrder(bob, plixUsdPair, OrderType.SELL, 100.plix, 800, matcherFee).message.id
      node.waitOrderStatus(plixUsdPair, orderId, "Accepted", 1.minute)

      node.cancelOrderWithApiKey(orderId)
      node.waitOrderStatus(plixUsdPair, orderId, "Cancelled", 1.minute)

      node.fullOrderHistory(bob).filter(_.id == orderId).head.status shouldBe "Cancelled"
      node.orderHistoryByPair(bob, plixUsdPair).filter(_.id == orderId).head.status shouldBe "Cancelled"

      val orderBook = node.orderBook(plixUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Cancel is rejected" - {
    "when request sender is not the sender of and order" in {
      val orderId = node.placeOrder(bob, plixUsdPair, OrderType.SELL, 100.plix, 800, matcherFee).message.id
      node.waitOrderStatus(plixUsdPair, orderId, "Accepted", 1.minute)

      node.expectCancelRejected(matcher, plixUsdPair, orderId)

      // Cleanup
      node.cancelOrder(bob, plixUsdPair, orderId)
      node.waitOrderStatus(plixUsdPair, orderId, "Cancelled")
    }
  }

  "Batch cancel" - {
    "works for" - {
      "all orders placed by an address" in {
        node.fullOrderHistory(bob)

        val usdOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, plixUsdPair, OrderType.SELL, 100.plix + i, 400, matcherFee).message.id
        }

        node.assetBalance(bob.toAddress.stringRepr, BtcId.toString)

        val btcOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, plixBtcPair, OrderType.BUY, 100.plix + i, 400, matcherFee).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => node.waitOrderStatus(plixUsdPair, id, "Accepted"))

        node.cancelAllOrders(bob)

        (usdOrderIds ++ btcOrderIds).foreach(id => node.waitOrderStatus(plixUsdPair, id, "Cancelled"))
      }

      "a pair" in {
        val usdOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, plixUsdPair, OrderType.SELL, 100.plix + i, 400, matcherFee).message.id
        }

        val btcOrderIds = 1 to 5 map { i =>
          node.placeOrder(bob, plixBtcPair, OrderType.BUY, 100.plix + i, 400, matcherFee).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => node.waitOrderStatus(plixUsdPair, id, "Accepted"))

        node.cancelOrdersForPair(bob, plixBtcPair)

        btcOrderIds.foreach(id => node.waitOrderStatus(plixUsdPair, id, "Cancelled"))
        usdOrderIds.foreach(id => node.waitOrderStatus(plixUsdPair, id, "Accepted"))
      }
    }
  }
}
