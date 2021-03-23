package com.plixplatform.it.sync.smartcontracts

import com.plixplatform.api.http.ApiError.TransactionNotAllowedByAccountScript
import com.plixplatform.common.state.ByteStr
import com.plixplatform.it.MatcherSuiteBase
import com.plixplatform.it.api.SyncHttpApi._
import com.plixplatform.it.api.SyncMatcherHttpApi._
import com.plixplatform.it.sync._
import com.plixplatform.it.sync.config.MatcherPriceAssetConfig._
import com.plixplatform.it.util._
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import play.api.libs.json.Json

import scala.concurrent.duration._

class OrderTypeTestSuite extends MatcherSuiteBase {
  private val aliceAsset =
    node
      .broadcastIssue(alice, "AliceCoinOrders", "AliceCoin for tests with order types", someAssetAmount, 0, reissuable = false, smartIssueFee, None)
      .id

  Seq(aliceAsset, node.broadcastRequest(IssueUsdTx.json()).id).map(node.waitForTransaction(_))

  private val predefAssetPair = plixUsdPair
  private val alicePlixPair  = AssetPair(IssuedAsset(ByteStr.decodeBase58(aliceAsset).get), Plix)

  "Order types verification with SmartContracts" - {
    val sco1 = s"""
                 |{-# STDLIB_VERSION 2 #-}
                 |match tx {
                 | case o : Order =>
                 |   o.orderType == Buy
                 | case s : SetScriptTransaction => true
                 | case other => throw()
                 | }
                 |""".stripMargin

    val sco2 = s"""
              |{-# STDLIB_VERSION 2 #-}
              |match tx {
              | case o : Order =>
              |    o.orderType == Sell
              |  case s : SetScriptTransaction => true
              |  case _ => throw()
              | }
      """.stripMargin

    val sco3 = s"""
                 |{-# STDLIB_VERSION 2 #-}
                 |match tx {
                 |  case o : Order =>
                 |        o.orderType == Buy || o.orderType == Sell
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    "scenarios of order placement" - {
      "set contracts with only BUY type and then place order" in {
        setContract(Some(sco1), alice)

        val aliceOrd1 = node
          .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.plix * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        node.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

        assertBadRequest(
          node
            .placeOrder(alice, alicePlixPair, OrderType.SELL, 500, 2.plix * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id)

        node.cancelOrder(alice, predefAssetPair, aliceOrd1).status should be("OrderCanceled")

        setContract(None, alice)
      }

      "set contracts with only SELL type and then place order" in {
        setContract(Some(sco2), alice)

        assertBadRequest(
          node
            .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.plix * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id)

        val aliceOrd2 = node
          .placeOrder(alice, alicePlixPair, OrderType.SELL, 500, 2.plix * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        node.waitOrderStatus(alicePlixPair, aliceOrd2, "Accepted", 1.minute)

        node.cancelOrder(alice, alicePlixPair, aliceOrd2).status should be("OrderCanceled")

        setContract(None, alice)
      }

      "set contracts with both SELL/BUY types and then place order" in {
        setContract(Some(sco3), alice)

        val aliceOrd1 = node
          .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.plix * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        node.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

        val aliceOrd2 = node
          .placeOrder(alice, alicePlixPair, OrderType.SELL, 500, 2.plix * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        node.waitOrderStatus(alicePlixPair, aliceOrd2, "Accepted", 1.minute)

        node.cancelOrder(alice, predefAssetPair, aliceOrd1).status should be("OrderCanceled")
        node.cancelOrder(alice, alicePlixPair, aliceOrd2).status should be("OrderCanceled")

        setContract(None, alice)
      }

      "place order and then set contract on BUY type" in {
        val aliceOrd1 = node
          .placeOrder(alice, predefAssetPair, OrderType.BUY, 500, 2.plix * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        node.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

        val aliceOrd2 = node
          .placeOrder(alice, alicePlixPair, OrderType.SELL, 500, 2.plix * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        node.waitOrderStatus(alicePlixPair, aliceOrd2, "Accepted", 1.minute)

        setContract(Some(sco1), alice)

        val bobOrd1 = node
          .placeOrder(bob, predefAssetPair, OrderType.SELL, 500, 2.plix * Order.PriceConstant, smartMatcherFee, version = 1, 10.minutes)
          .message
          .id
        val bobOrd2 = node
          .placeOrder(bob, alicePlixPair, OrderType.BUY, 500, 2.plix * Order.PriceConstant, smartMatcherFee, version = 1, 10.minutes)
          .message
          .id

        node.waitOrderStatus(predefAssetPair, aliceOrd1, "Filled", 1.minute)
        node.waitOrderStatus(alicePlixPair, aliceOrd2, "Filled", 1.minute)
        node.waitOrderStatus(predefAssetPair, bobOrd1, "Filled", 1.minute)
        node.waitOrderStatus(alicePlixPair, bobOrd2, "Filled", 1.minute)

        node.waitOrderInBlockchain(bobOrd1)

        val txs = node.waitTransactionsByOrder(bobOrd2, 1)
        node.expectSignedBroadcastRejected(Json.toJson(txs.head)) shouldBe TransactionNotAllowedByAccountScript.ErrorCode
      }
    }
  }
}
