package com.plixplatform.it.sync

import com.plixplatform.it.MatcherSuiteBase
import com.plixplatform.it.api.LevelResponse
import com.plixplatform.it.api.SyncHttpApi._
import com.plixplatform.it.api.SyncMatcherHttpApi._
import com.plixplatform.it.sync.config.MatcherPriceAssetConfig._
import com.plixplatform.transaction.assets.exchange.OrderType.BUY
import com.plixplatform.transaction.assets.exchange.{Order, OrderType}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode

class SeveralPartialOrdersTestSuite extends MatcherSuiteBase {
  override protected def beforeAll(): Unit = {
    super.beforeAll()
    node.waitForTransaction(node.broadcastRequest(IssueUsdTx.json()).id)
  }

  "Alice and Bob trade PLIX-USD" - {
    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 840340L

    "place usd-plix order" in {
      // Alice wants to sell USD for Plix
      val bobPlixBalanceBefore = node.accountBalances(bob.address)._1

      val bobOrder1   = node.prepareOrder(bob, plixUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder1Id = node.placeOrder(bobOrder1).message.id
      node.waitOrderStatus(plixUsdPair, bobOrder1Id, "Accepted", 1.minute)
      node.reservedBalance(bob)("PLIX") shouldBe sellOrderAmount + matcherFee
      node.tradableBalance(bob, plixUsdPair)("PLIX") shouldBe bobPlixBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder   = node.prepareOrder(alice, plixUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrderId = node.placeOrder(aliceOrder).message.id
      node.waitOrderStatus(plixUsdPair, aliceOrderId, "Filled", 1.minute)

      val aliceOrder2   = node.prepareOrder(alice, plixUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrder2Id = node.placeOrder(aliceOrder2).message.id
      node.waitOrderStatus(plixUsdPair, aliceOrder2Id, "Filled", 1.minute)

      // Bob wants to buy some USD
      node.waitOrderStatus(plixUsdPair, bobOrder1Id, "Filled", 1.minute)

      // Each side get fair amount of assets
      node.waitOrderInBlockchain(bobOrder1Id)
      node.reservedBalance(bob) shouldBe empty
      node.reservedBalance(alice) shouldBe empty

      // Previously cancelled order should not affect new orders
      val orderBook1 = node.orderBook(plixUsdPair)
      orderBook1.asks shouldBe empty
      orderBook1.bids shouldBe empty

      val bobOrder2   = node.prepareOrder(bob, plixUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder2Id = node.placeOrder(bobOrder2).message.id
      node.waitOrderStatus(plixUsdPair, bobOrder2Id, "Accepted", 1.minute)

      val orderBook2 = node.orderBook(plixUsdPair)
      orderBook2.asks shouldBe List(LevelResponse(bobOrder2.amount, bobOrder2.price))
      orderBook2.bids shouldBe empty

      node.cancelOrder(bob, plixUsdPair, bobOrder2Id)
      node.waitOrderStatus(plixUsdPair, bobOrder2Id, "Cancelled", 1.minute)

      node.reservedBalance(bob) shouldBe empty
      node.reservedBalance(alice) shouldBe empty
    }

    "place one submitted orders and two counter" in {
      val aliceOrder1   = node.prepareOrder(alice, plixUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrder1Id = node.placeOrder(aliceOrder1).message.id

      val aliceOrder2   = node.prepareOrder(alice, plixUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrder2Id = node.placeOrder(aliceOrder2).message.id

      val bobOrder1   = node.prepareOrder(bob, plixUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder1Id = node.placeOrder(bobOrder1).message.id

      node.waitOrderStatus(plixUsdPair, aliceOrder1Id, "Filled", 1.minute)
      node.waitOrderStatus(plixUsdPair, aliceOrder2Id, "Filled", 1.minute)
      node.waitOrderStatus(plixUsdPair, bobOrder1Id, "Filled", 1.minute)

      // Each side get fair amount of assets
      node.waitOrderInBlockchain(bobOrder1Id)
      node.reservedBalance(bob) shouldBe empty
      node.reservedBalance(alice) shouldBe empty

      // Previously cancelled order should not affect new orders
      val orderBook1 = node.orderBook(plixUsdPair)
      orderBook1.asks shouldBe empty
      orderBook1.bids shouldBe empty

      val bobOrder2   = node.prepareOrder(bob, plixUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder2Id = node.placeOrder(bobOrder2).message.id
      node.waitOrderStatus(plixUsdPair, bobOrder2Id, "Accepted", 1.minute)

      val orderBook2 = node.orderBook(plixUsdPair)
      orderBook2.asks shouldBe List(LevelResponse(bobOrder2.amount, bobOrder2.price))
      orderBook2.bids shouldBe empty
    }
  }

  def correctAmount(a: Long, price: Long): Long = {
    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  def receiveAmount(ot: OrderType, matchAmount: Long, matchPrice: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}
