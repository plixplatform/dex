package com.plixplatform.dex.model

import com.plixplatform.NoShrink
import com.plixplatform.dex.MatcherTestData
import com.plixplatform.dex.market.MatcherActor.OrderBookCreated
import com.plixplatform.dex.model.EventSerializers._
import com.plixplatform.transaction.assets.exchange.AssetPair
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class EventJsonSpecification extends PropSpec with PropertyChecks with Matchers with MatcherTestData with NoShrink {

  val buyLevelGen: Gen[Vector[BuyLimitOrder]] =
    Gen.containerOf[Vector, BuyLimitOrder](buyLimitOrderGenerator)

  val sellLevelGen: Gen[Vector[SellLimitOrder]] =
    Gen.containerOf[Vector, SellLimitOrder](sellLimitOrderGenerator)

  property("Write/Read OrderBook and Snapshot") {
    pending
  }

  property("OrderBookCreated json serialization roundtrip") {
    forAll(assetPairGen) { pair: AssetPair =>
      val obc = OrderBookCreated(pair)
      val js  = orderBookCreatedFormat.writes(obc)
      val r   = js.as[OrderBookCreated]
      obc shouldBe r
    }
  }
}
