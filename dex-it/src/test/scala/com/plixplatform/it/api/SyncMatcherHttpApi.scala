package com.plixplatform.it.api

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.plixplatform.account.KeyPair
import com.plixplatform.it.Node
import com.plixplatform.it.api.SyncHttpApi.RequestAwaitTime
import com.plixplatform.it.sync.config.MatcherPriceAssetConfig._
import com.plixplatform.dex.queue.QueueEventWithMeta
import com.plixplatform.transaction.Asset.Plix
import com.plixplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.plixplatform.transaction.{Asset, Proofs}
import org.asynchttpclient.util.HttpConstants
import org.asynchttpclient.{RequestBuilder, Response}
import org.scalatest.{Assertion, Assertions, Matchers}
import play.api.libs.json.Json.parse
import play.api.libs.json.{Format, Json, Writes}

import scala.concurrent.duration._
import scala.concurrent.{Await, Awaitable}
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

object SyncMatcherHttpApi extends Assertions {

  case class NotFoundErrorMessage(message: String)
  case class ErrorMessage(error: Int, message: String)

  object NotFoundErrorMessage {
    implicit val format: Format[NotFoundErrorMessage] = Json.format
  }

  def assertNotFoundAndMessage[R](f: => R, errorMessage: String): Assertion = Try(f) match {
    case Failure(UnexpectedStatusCodeException(_, _, statusCode, responseBody)) =>
      Assertions.assert(statusCode == StatusCodes.NotFound.intValue && parse(responseBody).as[NotFoundErrorMessage].message.contains(errorMessage))
    case Failure(e) => Assertions.fail(e)
    case _          => Assertions.fail(s"Expecting not found error")
  }

  def sync[A](awaitable: Awaitable[A], atMost: Duration = RequestAwaitTime): A =
    try Await.result(awaitable, atMost)
    catch {
      case usce: UnexpectedStatusCodeException => throw usce
      case NonFatal(cause) =>
        throw new Exception(cause)
    }

  implicit class MatcherNodeExtSync(m: Node) extends Matchers {

    import com.plixplatform.it.api.AsyncMatcherHttpApi.{MatcherAsyncHttpApi => async}

    private val RequestAwaitTime      = 30.seconds
    private val OrderRequestAwaitTime = 1.minutes

    def orderBook(assetPair: AssetPair): OrderBookResponse =
      sync(async(m).orderBook(assetPair))

    def orderBookExpectInvalidAssetId(assetPair: AssetPair, assetId: String): Boolean =
      Await.result(async(m).orderBookExpectInvalidAssetId(assetPair, assetId), OrderRequestAwaitTime)

    def orderBookExpectInvalidAssetId(assetPair: AssetPair, assetId: String, pred: MessageMatcherResponse => Boolean): Boolean =
      Await.result(async(m).orderBookExpectInvalidAssetId(assetPair, assetId, pred), OrderRequestAwaitTime)

    def orderStatusExpectInvalidAssetId(orderId: String, assetPair: AssetPair, assetId: String): Boolean =
      Await.result(async(m).orderStatusExpectInvalidAssetId(orderId, assetPair, assetId), OrderRequestAwaitTime)

    def orderStatusExpectInvalidAssetId(orderId: String, assetPair: AssetPair, assetId: String, pred: MessageMatcherResponse => Boolean): Boolean =
      Await.result(async(m).orderStatusExpectInvalidAssetId(orderId, assetPair, assetId, pred), OrderRequestAwaitTime)

    def marketStatus(assetPair: AssetPair): MarketStatusResponse =
      sync(async(m).marketStatus(assetPair), RequestAwaitTime)

    def deleteOrderBook(assetPair: AssetPair): MessageMatcherResponse =
      sync(async(m).deleteOrderBook(assetPair), RequestAwaitTime)

    def fullOrderHistory(sender: KeyPair): Seq[OrderbookHistory] =
      sync(async(m).fullOrdersHistory(sender), RequestAwaitTime)

    def orderHistoryByPair(sender: KeyPair, assetPair: AssetPair, activeOnly: Boolean = false): Seq[OrderbookHistory] =
      sync(async(m).orderHistoryByPair(sender, assetPair, activeOnly), RequestAwaitTime)

    def activeOrderHistory(sender: KeyPair): Seq[OrderbookHistory] =
      sync(async(m).fullOrdersHistory(sender, activeOnly = Some(true)))

    def placeOrder(order: Order): MatcherResponse =
      sync(async(m).placeOrder(order))

    def placeOrder(sender: KeyPair,
                   pair: AssetPair,
                   orderType: OrderType,
                   amount: Long,
                   price: Long,
                   fee: Long,
                   version: Byte = 1: Byte,
                   timeToLive: Duration = 30.days - 1.seconds): MatcherResponse =
      sync(async(m).placeOrder(sender, pair, orderType, amount, price, fee, version, timeToLive))

    def orderStatus(orderId: String, assetPair: AssetPair, waitForStatus: Boolean = true): MatcherStatusResponse =
      sync(async(m).orderStatus(orderId, assetPair, waitForStatus))

    def waitTransactionsByOrder(orderId: String, min: Int): Seq[ExchangeTransaction] =
      sync(async(m).waitTransactionsByOrder(orderId, min))

    def waitOrderStatus(assetPair: AssetPair,
                        orderId: String,
                        expectedStatus: String,
                        waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      sync(async(m).waitOrderStatus(assetPair, orderId, expectedStatus), waitTime)

    def waitOrderStatusAndAmount(assetPair: AssetPair,
                                 orderId: String,
                                 expectedStatus: String,
                                 expectedFilledAmount: Option[Long],
                                 waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      sync(async(m).waitOrderStatusAndAmount(assetPair, orderId, expectedStatus, expectedFilledAmount), waitTime)

    def waitOrderProcessed(assetPair: AssetPair, orderId: String, checkTimes: Int = 5, retryInterval: FiniteDuration = 1.second): Unit = {
      val fixedStatus = sync {
        async(m).waitFor[MatcherStatusResponse](s"$orderId processed")(
          _.orderStatus(orderId, assetPair),
          _.status != "NotFound",
          retryInterval
        )
      }

      // Wait until something changed or not :)
      def loop(n: Int): Unit =
        if (n == 0) m.log.debug(s"$orderId wasn't changed (tried $checkTimes times)")
        else {
          val currStatus = orderStatus(orderId, assetPair)
          if (currStatus == fixedStatus) {
            Thread.sleep(retryInterval.toMillis)
            loop(n - 1)
          } else m.log.debug(s"$orderId was changed on ${checkTimes - n} step")
        }

      loop(checkTimes)
    }

    def waitOrderInBlockchain(orderId: String,
                              retryInterval: FiniteDuration = 1.second,
                              waitTime: Duration = OrderRequestAwaitTime): Seq[TransactionInfo] =
      sync(async(m).waitForOrderInBlockchain(orderId, retryInterval), waitTime)

    def reservedBalance(sender: KeyPair, waitTime: Duration = OrderRequestAwaitTime): Map[String, Long] =
      sync(async(m).reservedBalance(sender), waitTime)

    def tradableBalance(sender: KeyPair, assetPair: AssetPair, waitTime: Duration = OrderRequestAwaitTime): Map[String, Long] =
      sync(async(m).tradableBalance(sender, assetPair), waitTime)

    def tradingMarkets(waitTime: Duration = OrderRequestAwaitTime): MarketDataInfo =
      sync(async(m).tradingMarkets(), waitTime)

    def expectIncorrectOrderPlacement(order: Order,
                                      expectedStatusCode: Int,
                                      expectedStatus: String,
                                      expectedMessage: Option[String] = None,
                                      waitTime: Duration = OrderRequestAwaitTime): Boolean =
      sync(async(m).expectIncorrectOrderPlacement(order, expectedStatusCode, expectedStatus, expectedMessage), waitTime)

    def expectRejectedOrderPlacement(sender: KeyPair,
                                     pair: AssetPair,
                                     orderType: OrderType,
                                     amount: Long,
                                     price: Long,
                                     fee: Long = 300000L,
                                     version: Byte = 1,
                                     timeToLive: Duration = 30.days - 1.seconds,
                                     expectedMessage: Option[String] = None): Boolean =
      expectIncorrectOrderPlacement(prepareOrder(sender, pair, orderType, amount, price, fee, version, timeToLive),
                                    400,
                                    "OrderRejected",
                                    expectedMessage)

    def expectCancelRejected(sender: KeyPair, assetPair: AssetPair, orderId: String, waitTime: Duration = OrderRequestAwaitTime): Unit =
      sync(async(m).expectCancelRejected(sender, assetPair, orderId), waitTime)

    def cancelOrder(sender: KeyPair, assetPair: AssetPair, orderId: String, waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      sync(async(m).cancelOrder(sender, assetPair, orderId), waitTime)

    def cancelOrdersForPair(sender: KeyPair,
                            assetPair: AssetPair,
                            timestamp: Long = System.currentTimeMillis(),
                            waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      sync(async(m).cancelOrdersForPair(sender, assetPair, timestamp), waitTime)

    def cancelAllOrders(sender: KeyPair,
                        timestamp: Long = System.currentTimeMillis(),
                        waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      sync(async(m).cancelAllOrders(sender, timestamp), waitTime)

    def cancelOrderWithApiKey(orderId: String, waitTime: Duration = OrderRequestAwaitTime): MatcherStatusResponse =
      sync(async(m).cancelOrderWithApiKey(orderId), waitTime)

    def matcherGet(path: String,
                   f: RequestBuilder => RequestBuilder = identity,
                   statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200,
                   waitForStatus: Boolean = false,
                   waitTime: Duration = RequestAwaitTime): Response =
      sync(async(m).matcherGet(path, f, statusCode, waitForStatus), waitTime)

    def matcherGetStatusCode(path: String, statusCode: Int, waitTime: Duration = RequestAwaitTime): MessageMatcherResponse =
      sync(async(m).matcherGetStatusCode(path, statusCode), waitTime)

    def matcherPost[A: Writes](path: String, body: A, waitTime: Duration = RequestAwaitTime): Response =
      sync(async(m).matcherPost(path, body), waitTime)

    def prepareOrder(sender: KeyPair,
                     pair: AssetPair,
                     orderType: OrderType,
                     amount: Long,
                     price: Long,
                     fee: Long = 300000L,
                     version: Byte = 1: Byte,
                     timeToLive: Duration = 30.days - 1.seconds,
                     matcherFeeAssetId: Asset = Plix): Order = {
      val creationTime        = System.currentTimeMillis()
      val timeToLiveTimestamp = creationTime + timeToLive.toMillis

      val unsigned =
        Order(
          sender,
          matcher,
          pair,
          orderType,
          amount,
          price,
          creationTime,
          timeToLiveTimestamp,
          fee,
          Proofs.empty,
          version,
          matcherFeeAssetId
        )
      Order.sign(unsigned, sender)
    }

    def ordersByAddress(sender: KeyPair, activeOnly: Boolean, waitTime: Duration = RequestAwaitTime): Seq[OrderbookHistory] =
      sync(async(m).ordersByAddress(sender, activeOnly), waitTime)

    def getCurrentOffset: QueueEventWithMeta.Offset                   = sync(async(m).getCurrentOffset)
    def getLastOffset: QueueEventWithMeta.Offset                      = sync(async(m).getLastOffset)
    def getOldestSnapshotOffset: QueueEventWithMeta.Offset            = sync(async(m).getOldestSnapshotOffset)
    def getAllSnapshotOffsets: Map[String, QueueEventWithMeta.Offset] = sync(async(m).getAllSnapshotOffsets)

    def waitForStableOffset(confirmations: Int,
                            maxTries: Int,
                            interval: FiniteDuration,
                            waitTime: Duration = RequestAwaitTime): QueueEventWithMeta.Offset =
      sync(async(m).waitForStableOffset(confirmations, maxTries, interval), (maxTries + 1) * interval)

    def matcherState(assetPairs: Seq[AssetPair],
                     orders: IndexedSeq[Order],
                     accounts: Seq[KeyPair],
                     waitTime: Duration = RequestAwaitTime * 5): MatcherState =
      sync(async(m).matcherState(assetPairs, orders, accounts), waitTime)

    def upsertRate[A: Writes](asset: Asset, rate: Double, waitTime: Duration = RequestAwaitTime, expectedStatusCode: StatusCode): RatesResponse =
      sync(async(m).upsertRate(asset, rate, expectedStatusCode.intValue), waitTime)

    def getRates: Map[Asset, Double] = sync(async(m).getRates())

    def deleteRate(asset: Asset, expectedStatusCode: StatusCode = StatusCodes.OK): RatesResponse =
      sync(async(m).deleteRate(asset, expectedStatusCode.intValue))
  }
}
