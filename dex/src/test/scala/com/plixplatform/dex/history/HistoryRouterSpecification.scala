package com.plixplatform.dex.history

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.plixplatform.NTPTime
import com.plixplatform.account.{KeyPair, PublicKey}
import com.plixplatform.common.state.ByteStr
import com.plixplatform.dex.MatcherTestData
import com.plixplatform.dex.history.HistoryRouter.{SaveEvent, SaveOrder}
import com.plixplatform.dex.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.plixplatform.dex.model.LimitOrder
import com.plixplatform.dex.model.MatcherModel.Denormalization
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV1}
import com.plixplatform.wallet.Wallet
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class HistoryRouterSpecification
    extends TestKit(ActorSystem("AddressActorSpecification"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with NTPTime
    with MatcherTestData {

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }

  def privateKey(seed: String): KeyPair = Wallet.generateNewAccount(seed.getBytes(), 0)

  val assetId    = ByteStr("asset".getBytes)
  val matcherFee = 30000L

  val assetDecimals: Byte = 8
  val plixDecimals: Byte = 8

  val sender0Seed = "test"
  val sender1Seed = "test1"
  val sender2Seed = "test2"
  val sender3Seed = "test3"

  val buyPlixOrder   = getOrder(sender0Seed, OrderType.BUY, 300L, 1L)
  val sellPlixOrder1 = getOrder(sender1Seed, OrderType.SELL, 100L, 2L)
  val sellPlixOrder2 = getOrder(sender2Seed, OrderType.SELL, 100L, 3L)
  val sellPlixOrder3 = getOrder(sender3Seed, OrderType.SELL, 100L, 4L)

  val buyPlixOrderCancelled = getOrder(sender0Seed, OrderType.BUY, 300L, 5L)

  val buyPlixOrderFilledAndCancelled = getOrder(sender0Seed, OrderType.BUY, 300L, 6L)
  val sellPlixOrder4                 = getOrder(sender1Seed, OrderType.SELL, 100L, 7L)

  val sellPlixOrderFilling           = getOrder(sender1Seed, OrderType.SELL, 100L, 7L)
  val buyPlixOrderFilledAfterPlacing = getOrder(sender0Seed, OrderType.BUY, 100L, 8L)

  def getOrder(senderSeed: String, orderType: OrderType, amount: Long, timestamp: Long): LimitOrder = {
    LimitOrder(
      OrderV1(
        sender = privateKey(senderSeed),
        matcher = PublicKey("matcher".getBytes()),
        pair = AssetPair(Plix, IssuedAsset(assetId)),
        orderType = orderType,
        price = Order.PriceConstant,
        amount = amount * Order.PriceConstant,
        timestamp = timestamp,
        expiration = 1000L,
        matcherFee = matcherFee
      )
    )
  }

  def orderAdded(submitted: LimitOrder): OrderAdded                            = OrderAdded(submitted, ntpTime.getTimestamp())
  def orderExecuted(submitted: LimitOrder, counter: LimitOrder): OrderExecuted = OrderExecuted(submitted, counter, ntpTime.getTimestamp())
  def orderCancelled(submitted: LimitOrder): OrderCanceled                     = OrderCanceled(submitted, false, ntpTime.getTimestamp())

  // don't need to use blockchain in order to find out asset decimals, therefore pair parameter isn't used
  def denormalizeAmountAndFee(value: Long, pair: AssetPair): Double = Denormalization.denormalizeAmountAndFee(value, plixDecimals)
  def denormalizePrice(value: Long, pair: AssetPair): Double        = Denormalization.denormalizePrice(value, plixDecimals, assetDecimals)

  implicit class LimitOrderOps(limitOrder: LimitOrder) {
    def orderId: String = limitOrder.order.id().toString
    def senderPublicKey: String = limitOrder.order.senderPublicKey.toString
  }

  case class OrderShortenedInfo(id: String, senderPublicKey: String, side: Byte, price: Double, amount: Double)
  case class EventShortenedInfo(orderId: String, eventType: Byte, filled: Double, totalFilled: Double, status: Byte)

  def getOrderInfo(orderAddedEvent: OrderAdded): OrderShortenedInfo = {
    SaveOrder(orderAddedEvent.order, orderAddedEvent.timestamp)
      .createRecords(denormalizeAmountAndFee, denormalizePrice)
      .map(r => OrderShortenedInfo(r.id, r.senderPublicKey, r.side, r.price, r.amount))
      .head
  }

  def getEventsInfo(event: Event): Set[EventShortenedInfo] = {
    SaveEvent(event)
      .createRecords(denormalizeAmountAndFee, denormalizePrice)
      .map(r => EventShortenedInfo(r.orderId, r.eventType, r.filled, r.totalFilled, r.status))
  }

  "HistoryRouter" should {
    "correctly convert events to records" in {

      import HistoryRouter._

      // place big buy order
      getOrderInfo(orderAdded(buyPlixOrder)) shouldBe
        OrderShortenedInfo(buyPlixOrder.orderId, buyPlixOrder.senderPublicKey, buySide, price = 1, amount = 300)

      // place small sell order 1
      getOrderInfo(orderAdded(sellPlixOrder1)) shouldBe
        OrderShortenedInfo(sellPlixOrder1.orderId, sellPlixOrder1.senderPublicKey, sellSide, price = 1, amount = 100)

      // big buy order executed first time
      val orderExecutedEvent1 = orderExecuted(buyPlixOrder, sellPlixOrder1)
      getEventsInfo(orderExecutedEvent1) shouldBe Set(
        EventShortenedInfo(buyPlixOrder.orderId, eventTrade, filled = 100, totalFilled = 100, statusPartiallyFilled),
        EventShortenedInfo(sellPlixOrder1.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled)
      )

      // place small sell order 2
      getOrderInfo(orderAdded(sellPlixOrder2)) shouldBe
        OrderShortenedInfo(sellPlixOrder2.orderId, sellPlixOrder2.senderPublicKey, sellSide, price = 1, amount = 100)

      // big buy order executed second time
      val orderExecutedEvent2 = orderExecuted(orderExecutedEvent1.submittedRemaining, sellPlixOrder2)
      getEventsInfo(orderExecutedEvent2) shouldBe Set(
        EventShortenedInfo(buyPlixOrder.orderId, eventTrade, filled = 100, totalFilled = 200, statusPartiallyFilled),
        EventShortenedInfo(sellPlixOrder2.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled)
      )

      // place small sell order 3
      getOrderInfo(orderAdded(sellPlixOrder3)) shouldBe
        OrderShortenedInfo(sellPlixOrder3.orderId, sellPlixOrder3.senderPublicKey, sellSide, price = 1, amount = 100)

      // big buy order executed third time and filled
      val orderExecutedEvent3 = orderExecuted(orderExecutedEvent2.submittedRemaining, sellPlixOrder3)
      getEventsInfo(orderExecutedEvent3) shouldBe Set(
        EventShortenedInfo(buyPlixOrder.orderId, eventTrade, filled = 100, totalFilled = 300, statusFilled),
        EventShortenedInfo(sellPlixOrder3.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled)
      )

      // place order and then cancel
      getOrderInfo(orderAdded(buyPlixOrderCancelled)) shouldBe
        OrderShortenedInfo(buyPlixOrderCancelled.orderId, buyPlixOrderCancelled.senderPublicKey, buySide, price = 1, amount = 300)

      getEventsInfo(orderCancelled(buyPlixOrderCancelled)) shouldBe Set(
        EventShortenedInfo(buyPlixOrderCancelled.orderId, eventCancel, filled = 0, totalFilled = 0, statusCancelled),
      )

      // place buy order
      getOrderInfo(orderAdded(buyPlixOrderFilledAndCancelled)) shouldBe
        OrderShortenedInfo(buyPlixOrderFilledAndCancelled.orderId, buyPlixOrderFilledAndCancelled.senderPublicKey, buySide, price = 1, amount = 300)

      // place sell order
      getOrderInfo(orderAdded(sellPlixOrder4)) shouldBe
        OrderShortenedInfo(sellPlixOrder4.orderId, sellPlixOrder4.senderPublicKey, sellSide, price = 1, amount = 100)

      // buy order partially filled
      val cancellingOrderExecutedEvent = orderExecuted(buyPlixOrderFilledAndCancelled, sellPlixOrder4)
      getEventsInfo(cancellingOrderExecutedEvent) shouldBe Set(
        EventShortenedInfo(buyPlixOrderFilledAndCancelled.orderId, eventTrade, filled = 100, totalFilled = 100, statusPartiallyFilled),
        EventShortenedInfo(sellPlixOrder4.orderId, eventTrade, filled = 100, totalFilled = 100, statusFilled)
      )

      // buy order cancelled
      getEventsInfo(orderCancelled(cancellingOrderExecutedEvent.submittedRemaining)) shouldBe Set(
        EventShortenedInfo(buyPlixOrderFilledAndCancelled.orderId, eventCancel, filled = 0, totalFilled = 100, statusCancelled),
      )
    }
  }
}
