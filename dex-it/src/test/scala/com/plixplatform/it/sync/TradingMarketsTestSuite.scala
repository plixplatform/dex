package com.plixplatform.it.sync

import com.plixplatform.it.MatcherSuiteBase
import com.plixplatform.it.api.SyncHttpApi._
import com.plixplatform.it.api.SyncMatcherHttpApi._
import com.plixplatform.it.sync.config.MatcherPriceAssetConfig._
import com.plixplatform.transaction.assets.exchange.OrderType.BUY

class TradingMarketsTestSuite extends MatcherSuiteBase {
  val (amount, price) = (1000L, 1000000000L)

  "When some orders were placed and matcher was restarted" - {
    "Trading markets have info about all asset pairs" in {
      val wctTxId = node.broadcastRequest(IssueWctTx.json()).id
      node.waitForTransaction(wctTxId)
      node.waitForHeight(node.height + 1)

      val order = node.placeOrder(alice, wctPlixPair, BUY, amount, price, matcherFee).message.id
      node.waitOrderStatus(wctPlixPair, order, "Accepted")

      docker.restartNode(node)

      val markets = node.tradingMarkets().markets
      markets.size shouldBe 1
      markets.head.amountAssetName shouldNot be("Unknown")
      markets.head.priceAssetName shouldNot be("Unknown")
    }
  }
}
