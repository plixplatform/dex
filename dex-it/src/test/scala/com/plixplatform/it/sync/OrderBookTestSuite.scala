package com.plixplatform.it.sync

import com.plixplatform.account.KeyPair
import com.plixplatform.it.MatcherSuiteBase
import com.plixplatform.it.api.SyncHttpApi._
import com.plixplatform.it.api.SyncMatcherHttpApi
import com.plixplatform.it.api.SyncMatcherHttpApi._
import com.plixplatform.it.sync.config.MatcherPriceAssetConfig._
import com.plixplatform.transaction.assets.exchange.Order.PriceConstant
import com.plixplatform.transaction.assets.exchange.OrderType._

class OrderBookTestSuite extends MatcherSuiteBase {

  {
    val xs = Seq(IssueUsdTx, IssueWctTx).map(_.json()).map(node.broadcastRequest(_))
    xs.foreach(x => node.waitForTransaction(x.id))
    node.waitForHeight(node.height + 1)
  }

  case class ReservedBalances(wct: Long, usd: Long, plix: Long)
  def reservedBalancesOf(pk: KeyPair): ReservedBalances = {
    val reservedBalances = node.reservedBalance(pk)
    ReservedBalances(
      reservedBalances.getOrElse(WctId.toString, 0),
      reservedBalances.getOrElse(UsdId.toString, 0),
      reservedBalances.getOrElse("PLIX", 0)
    )
  }

  val (amount, price) = (1000L, PriceConstant)

  "When delete order book" - {
    val buyOrder        = node.placeOrder(alice, wctUsdPair, BUY, 2 * amount, price, matcherFee).message.id
    val anotherBuyOrder = node.placeOrder(alice, wctUsdPair, BUY, amount, price, matcherFee).message.id

    val submitted = node.placeOrder(bob, wctUsdPair, SELL, amount, price, matcherFee).message.id

    val sellOrder = node.placeOrder(bob, wctUsdPair, SELL, amount, 2 * price, matcherFee).message.id

    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, submitted, "Filled")

    val (aliceRBForOnePair, bobRBForOnePair) = (reservedBalancesOf(alice), reservedBalancesOf(bob))

    val buyOrderForAnotherPair = node.placeOrder(alice, wctPlixPair, BUY, amount, price, matcherFee).message.id
    val sellOrderForAnotherPair =
      node.placeOrder(bob, wctPlixPair, SELL, amount, 2 * price, matcherFee).message.id

    node.waitOrderStatus(wctPlixPair, buyOrderForAnotherPair, "Accepted")
    node.waitOrderStatus(wctPlixPair, sellOrderForAnotherPair, "Accepted")

    val (aliceRBForBothPairs, bobRBForBothPairs) = (reservedBalancesOf(alice), reservedBalancesOf(bob))

    node.deleteOrderBook(wctUsdPair)

    "orders by the pair should be canceled" in {
      node.waitOrderStatus(wctUsdPair, buyOrder, "Cancelled")
      node.waitOrderStatus(wctUsdPair, anotherBuyOrder, "Cancelled")
      node.waitOrderStatus(wctUsdPair, sellOrder, "Cancelled")
    }

    "orderbook was deleted" in {
      withClue("orderBook") {
        val orderBook = node.orderBook(wctUsdPair)
        orderBook.bids shouldBe empty
        orderBook.asks shouldBe empty
      }

      withClue("tradingMarkets") {
        val tradingPairs = node.tradingMarkets().markets.map(x => s"${x.amountAsset}-${x.priceAsset}")
        tradingPairs shouldNot contain(wctUsdPair.key)
      }

      withClue("getAllSnapshotOffsets") {
        node.getAllSnapshotOffsets.keySet shouldNot contain(wctUsdPair.key)
      }
    }

    "reserved balances should be released for the pair" in {
      val (aliceReservedBalances, bobReservedBalances) = (reservedBalancesOf(alice), reservedBalancesOf(bob))
      aliceReservedBalances.usd shouldBe 0
      aliceReservedBalances.plix shouldBe (aliceRBForBothPairs.plix - aliceRBForOnePair.plix)
      bobReservedBalances.wct shouldBe (bobRBForBothPairs.wct - bobRBForOnePair.wct)
      bobReservedBalances.plix shouldBe (bobRBForBothPairs.plix - bobRBForOnePair.plix)
    }

    "it should not affect other pairs and their orders" in {
      node.orderStatus(buyOrderForAnotherPair, wctPlixPair).status shouldBe "Accepted"
      node.orderStatus(sellOrderForAnotherPair, wctPlixPair).status shouldBe "Accepted"
      node.placeOrder(alice, wctPlixPair, BUY, amount, price, matcherFee)

      val orderBook = node.orderBook(wctPlixPair)
      orderBook.bids shouldNot be(empty)
      orderBook.asks shouldNot be(empty)
    }

    "matcher can start after multiple delete events" in {
      import com.plixplatform.it.api.AsyncMatcherHttpApi.{MatcherAsyncHttpApi => async}

      def deleteWctPlix = async(node).deleteOrderBook(wctPlixPair)
      val deleteMultipleTimes = deleteWctPlix
        .zip(deleteWctPlix)
        .map(_ => ())
        .recover { case _ => () } // It's ok: either this should fail, or restartNode should work

      SyncMatcherHttpApi.sync(deleteMultipleTimes)

      docker.restartNode(node)
    }
  }
}
