package com.plixplatform.dex.model

import com.plixplatform.account.KeyPair
import com.plixplatform.dex.MatcherTestData
import com.plixplatform.dex.db.OrderDB
import com.plixplatform.transaction.assets.exchange.{AssetPair, Order}
import com.plixplatform.{NoShrink, WithDB}
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class OrderDBSpec extends FreeSpec with Matchers with WithDB with MatcherTestData with PropertyChecks with NoShrink {
  import OrderDBSpec._

  private def finalizedOrderInfoGen(o: Order): Gen[(Order, OrderInfo[OrderStatus.Final])] =
    for {
      filledAmount <- Gen.choose(0, o.amount)
      filledFee    <- Gen.choose(0, o.matcherFee)
      status       <- Gen.oneOf(OrderStatus.Filled(o.amount, o.matcherFee), OrderStatus.Cancelled(filledAmount, filledFee))
    } yield o -> o.toInfo(status)

  private def finalizedOrderSeqGen(orderCount: Int): Gen[(KeyPair, AssetPair, Seq[(Order, OrderInfo[OrderStatus.Final])])] =
    for {
      sender    <- accountGen
      pair      <- distinctPairGen
      orderList <- Gen.listOfN(orderCount, orderGenerator(sender, pair).flatMap(o => finalizedOrderInfoGen(o)))
    } yield (sender, pair, orderList)

  private val finalizedOrderInfoGen: Gen[(Order, OrderInfo[OrderStatus.Final])] = for {
    (o, _) <- orderGenerator
    result <- finalizedOrderInfoGen(o)
  } yield result

  private def test(f: OrderDB => Any): Any = f(OrderDB(matcherSettings, db))

  "Default OrderDB implementation" - {
    "stores" - {
      "order" in test { odb =>
        forAll(orderGenerator) {
          case (o, _) =>
            odb.saveOrder(o)
        }
      }

      "order info for terminated orders" in test { odb =>
        forAll(finalizedOrderInfoGen) {
          case (o, oi) =>
            odb.saveOrderInfo(o.id(), o.sender, oi)
            odb.containsInfo(o.id()) shouldBe true
            odb.status(o.id()) shouldBe oi.status
        }
      }
    }

    "does not overwrite finalized info" in test { odb =>
      val dualFinalizedOrderInfoGen: Gen[(Order, OrderInfo[OrderStatus.Final], OrderInfo[OrderStatus.Final])] = for {
        (o, _)       <- orderGenerator
        filledAmount <- Gen.choose(0, o.amount)
        filledFee    <- Gen.choose(0, o.matcherFee)
        s1           <- Gen.oneOf(OrderStatus.Filled(o.amount, o.matcherFee), OrderStatus.Cancelled(filledAmount, filledFee))
        s2           <- Gen.oneOf(OrderStatus.Filled(o.amount, o.matcherFee), OrderStatus.Cancelled(filledAmount, filledFee))
      } yield
        (
          o,
          OrderInfo.v2(o.orderType, o.amount, o.price, o.matcherFee, o.matcherFeeAssetId, o.timestamp, s1, o.assetPair),
          OrderInfo.v2(o.orderType, o.amount, o.price, o.matcherFee, o.matcherFeeAssetId, o.timestamp, s2, o.assetPair),
        )

      forAll(dualFinalizedOrderInfoGen) {
        case (o, oi1, oi2) =>
          odb.saveOrderInfo(o.id(), o.sender, oi1)
          odb.saveOrderInfo(o.id(), o.sender, oi2)

          odb.status(o.id()) shouldBe oi1.status
      }
    }

    "loads remaining orders when limits are not exceeded" in test { odb =>
      forAll(finalizedOrderSeqGen(20)) {
        case (sender, pair, orders) =>
          for ((o, i) <- orders) {
            odb.saveOrder(o)
            odb.saveOrderInfo(o.id(), o.sender, i)
          }

          val tuples = odb.loadRemainingOrders(sender, Some(pair), Seq.empty)
          tuples should contain allElementsOf orders.map { case (o, i) => o.id() -> i }
      }
    }

    "does not load more orders when there are too many active orders" in {
      val odb = OrderDB(matcherSettings.copy(maxOrdersPerRequest = 30), db)
      val paramGen = for {
        (sender, pair, finalizedOrders) <- finalizedOrderSeqGen(20)
        activeOrders                    <- Gen.listOfN(20, orderGenerator(sender, pair))
      } yield (sender, pair, activeOrders.map(o => o.id() -> o.toInfo(OrderStatus.Accepted)), finalizedOrders)

      forAll(paramGen) {
        case (sender, pair, active, finalized) =>
          for ((o, i) <- finalized) {
            odb.saveOrder(o)
            odb.saveOrderInfo(o.id(), o.sender, i)
          }

          val loadedOrders = odb.loadRemainingOrders(sender, Some(pair), active)
          loadedOrders should contain allElementsOf active
          loadedOrders.size should be <= matcherSettings.maxOrdersPerRequest

      }
    }
  }
}

object OrderDBSpec {
  private implicit class OrderExt(val o: Order) extends AnyVal {
    def toInfo[A <: OrderStatus](status: A) =
      OrderInfo.v2[A](o.orderType, o.amount, o.price, o.matcherFee, o.matcherFeeAssetId, o.timestamp, status, o.assetPair)
  }
}
