package com.plixplatform.it.sync

import com.plixplatform.account.KeyPair
import com.plixplatform.common.state.ByteStr
import com.plixplatform.it.MatcherSuiteBase
import com.plixplatform.it.api.SyncHttpApi._
import com.plixplatform.it.api.SyncMatcherHttpApi._
import com.plixplatform.it.sync.config.MatcherPriceAssetConfig._
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._
import scala.util.Random

class MatcherMassOrdersTestSuite extends MatcherSuiteBase {
  private def orderVersion = (Random.nextInt(2) + 1).toByte

  "Create orders with statuses FILL, PARTIAL, CANCELLED, ACTIVE" - {
    // Alice issues new assets
    val aliceAsset = node
      .broadcastIssue(
        source = alice,
        name = "AliceCoin",
        description = "AliceCoin for matcher's tests",
        quantity = someAssetAmount,
        decimals = 0,
        reissuable = false,
        script = None,
        fee = issueFee
      )
      .id

    val aliceSecondAsset = node
      .broadcastIssue(
        source = alice,
        name = "AliceSecondCoin",
        description = "AliceSecondCoin for matcher's tests",
        quantity = someAssetAmount,
        decimals = 0,
        reissuable = false,
        script = None,
        fee = issueFee
      )
      .id

    Seq(aliceAsset, aliceSecondAsset).foreach(node.waitForTransaction(_))
    val alicePlixPair       = AssetPair(IssuedAsset(ByteStr.decodeBase58(aliceAsset).get), Plix)
    val aliceSecondPlixPair = AssetPair(IssuedAsset(ByteStr.decodeBase58(aliceSecondAsset).get), Plix)

    withClue("Check balances on Alice's account") {
      node.assertAssetBalance(alice.address, aliceAsset, someAssetAmount)
      node.assertAssetBalance(alice.address, aliceSecondAsset, someAssetAmount)
      node.assertAssetBalance(matcher.address, aliceAsset, 0)
    }

    withClue("Distribute assets") {
      val xs = Seq(
        node.broadcastTransfer(alice, bob.address, someAssetAmount / 2, minFee, Some(aliceAsset), None),
        node.broadcastTransfer(alice, bob.address, someAssetAmount / 2, minFee, Some(aliceSecondAsset), None),
      )
      xs.foreach(tx => node.waitForTransaction(tx.id))
    }

    node.assertAssetBalance(bob.address, aliceAsset, someAssetAmount / 2)
    node.assertAssetBalance(bob.address, aliceSecondAsset, someAssetAmount / 2)

    // Alice places sell orders
    val aliceOrderIdFill = node
      .placeOrder(alice, aliceSecondPlixPair, OrderType.SELL, 3, Order.PriceConstant, matcherFee, orderVersion, 10.minutes)
      .message
      .id

    val alicePartialOrderId = node
      .placeOrder(alice, aliceSecondPlixPair, OrderType.SELL, 3, Order.PriceConstant, matcherFee, orderVersion, 10.minutes)
      .message
      .id

    val aliceOrderToCancelId =
      node
        .placeOrder(alice, aliceSecondPlixPair, OrderType.SELL, 3, Order.PriceConstant, matcherFee, orderVersion, 70.seconds)
        .message
        .id

    val aliceActiveOrderId = node
      .placeOrder(alice, aliceSecondPlixPair, OrderType.SELL, 3, Order.PriceConstant + 100000000, matcherFee, orderVersion, 10.minutes)
      .message
      .id

    node.cancelOrder(alice, aliceSecondPlixPair, aliceOrderToCancelId) // TODO: remove this line in DEX-160
    node.waitOrderStatus(aliceSecondPlixPair, aliceOrderToCancelId, "Cancelled", 2.minutes)

    //Bob orders should partially fill one Alice order and fill another
    ordersRequestsGen(2, bob, aliceSecondPlixPair, OrderType.BUY, 2)

    //check orders after filling
    node.waitOrderStatus(aliceSecondPlixPair, alicePartialOrderId, "PartiallyFilled")

    orderStatus(alice, aliceSecondPlixPair, aliceOrderIdFill, "Filled")
    orderStatus(alice, aliceSecondPlixPair, alicePartialOrderId, "PartiallyFilled")

    "Mass orders creation with random lifetime. Active orders still in list" in {
      node.ordersByAddress(alice, activeOnly = false).length shouldBe 4
      node.ordersByAddress(alice, activeOnly = true).length shouldBe 2

      node.ordersByAddress(bob, activeOnly = false).length shouldBe 2
      node.ordersByAddress(bob, activeOnly = true).length shouldBe 0

      val orderIds = node.fullOrderHistory(alice).map(_.id)

      orderIds should contain(aliceActiveOrderId)

      ordersRequestsGen(orderLimit + 1, alice, alicePlixPair, OrderType.SELL, 3)

      //wait for some orders cancelled
      Thread.sleep(5000)
      val bobsOrderIds = ordersRequestsGen(orderLimit + 1, bob, alicePlixPair, OrderType.BUY, 2)
      Thread.sleep(5000)

      // Alice check that order Active order is still in list
      val orderIdsAfterMatching = node.fullOrderHistory(alice).map(_.id)

      orderIdsAfterMatching should contain(aliceActiveOrderId)
      orderIdsAfterMatching should contain(alicePartialOrderId)

      node.waitOrderStatus(aliceSecondPlixPair, aliceActiveOrderId, "Accepted")
      node.waitOrderStatus(aliceSecondPlixPair, alicePartialOrderId, "PartiallyFilled")

      node.fullOrderHistory(bob).map(_.id) should equal(bobsOrderIds.drop(1).reverse)
      node.orderHistoryByPair(bob, alicePlixPair).map(_.id) should equal(bobsOrderIds.drop(1).reverse)
    }

    "Filled and Cancelled orders should be after Partial And Accepted" in {
      val lastIdxOfActiveOrder =
        node.fullOrderHistory(alice).lastIndexWhere(o => o.status.equals("Accepted") || o.status.equals("PartiallyFilled"))
      val firstIdxOfClosedOrder = node.fullOrderHistory(alice).indexWhere(o => o.status.equals("Filled") || o.status.equals("Cancelled"))
      lastIdxOfActiveOrder should be < firstIdxOfClosedOrder
    }

    "Accepted and PartiallyFilled orders should be sorted by timestamp." in {
      val activeAndPartialOrders =
        node.fullOrderHistory(alice).filter(o => o.status.equals("Accepted") || o.status.equals("PartiallyFilled")).map(_.timestamp)
      activeAndPartialOrders.reverse shouldBe sorted
    }

    "Filled and Cancelled orders should be sorted by timestamp." in {
      val filledAndCancelledOrders =
        node.fullOrderHistory(alice).filter(o => o.status.equals("Filled") || o.status.equals("Cancelled")).map(_.timestamp)
      filledAndCancelledOrders.reverse shouldBe sorted
    }

    "check order history orders count after fill" in {
      val aliceOrderHistory = node.fullOrderHistory(alice)
      aliceOrderHistory.size shouldBe orderLimit
      val aliceOrderHistoryByPair = node.orderHistoryByPair(alice, alicePlixPair)
      aliceOrderHistoryByPair.size shouldBe orderLimit
    }
  }

  private def ordersRequestsGen(n: Int, sender: KeyPair, assetPair: AssetPair, orderType: OrderType, amount: Long): Seq[String] = {
    val orderIds = 1 to n map (_ => {
      node
        .placeOrder(sender, assetPair, orderType, amount, Order.PriceConstant, matcherFee, orderVersion, (120 + Random.nextInt(70)).seconds)
        .message
        .id
    })
    orderIds
  }

  private def orderStatus(sender: KeyPair, assetPair: AssetPair, orderId: String, expectedStatus: String) =
    node.waitOrderStatus(assetPair, orderId, expectedStatus)
}
