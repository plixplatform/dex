package com.plixplatform.dexgen.utils

import java.io.IOException
import java.util.concurrent.TimeoutException

import com.google.common.primitives.Longs
import com.plixplatform.account.PrivateKey
import com.plixplatform.api.http.assets.{SignedIssueV2Request, SignedMassTransferRequest, SignedTransferV1Request}
import com.plixplatform.common.state.ByteStr
import com.plixplatform.crypto
import com.plixplatform.it.api.{
  AssetBalance,
  Balance,
  MatcherResponse,
  MatcherStatusResponse,
  OrderBookResponse,
  OrderbookHistory,
  ResponseFutureExt,
  Transaction,
  UnexpectedStatusCodeException
}
import com.plixplatform.it.util.GlobalTimer.{instance => timer}
import com.plixplatform.it.util._
import com.plixplatform.dex.api.CancelOrderRequest
import com.plixplatform.transaction.Asset
import com.plixplatform.transaction.Asset.Plix
import com.plixplatform.transaction.assets.IssueTransactionV2
import com.plixplatform.transaction.assets.exchange.{AssetPair, Order}
import com.plixplatform.transaction.transfer.MassTransferTransaction.{ParsedTransfer, Transfer}
import com.plixplatform.transaction.transfer.{MassTransferTransaction, TransferTransactionV1}
import com.plixplatform.utils.ScorexLogging
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import play.api.libs.json.Json.{stringify, toJson}
import play.api.libs.json._
import scorex.crypto.encode.Base58

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class ApiRequests(client: AsyncHttpClient) extends ScorexLogging {

  def retrying(r: Request, interval: FiniteDuration = 1.second, statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200)(
      implicit tag: String): Future[Response] = {
    def executeRequest: Future[Response] = {
      log.info(s"[$tag] Executing request '$r'")
      client
        .executeRequest(
          r,
          new AsyncCompletionHandler[Response] {
            override def onCompleted(response: Response): Response = {
              if (response.getStatusCode == statusCode) {
                log.info(s"[$tag] Request: ${r.getUrl}\nResponse: ${response.getResponseBody}")
                response
              } else {
                log.info(s"[$tag] Request: ${r.getUrl}\nUnexpected status code(${response.getStatusCode}): ${response.getResponseBody}")
                throw UnexpectedStatusCodeException(r.getMethod, r.getUrl, response.getStatusCode, response.getResponseBody)
              }
            }
          }
        )
        .toCompletableFuture
        .toScala
        .recoverWith {
          case e @ (_: IOException | _: TimeoutException) =>
            log.info(s"[$tag] Failed to execute request '$r' with error: ${e.getMessage}")
            timer.schedule(executeRequest, interval)
        }
    }

    executeRequest
  }

  def createSignedIssueRequest(tx: IssueTransactionV2): SignedIssueV2Request = {
    import tx._
    SignedIssueV2Request(
      Base58.encode(tx.sender),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      proofs.proofs.map(_.toString),
      script.map(_.toString)
    )
  }

  def createSignedMassTransferRequest(tx: MassTransferTransaction): SignedMassTransferRequest = {
    SignedMassTransferRequest(
      Base58.encode(tx.sender),
      tx.assetId.compatId.map(_.toString),
      tx.transfers.map { case ParsedTransfer(address, amount) => Transfer(address.stringRepr, amount) },
      tx.fee,
      tx.timestamp,
      tx.attachment.headOption.map(_ => Base58.encode(tx.attachment)),
      tx.proofs.base58().toList
    )
  }

  def createSignedTransferRequest(tx: TransferTransactionV1): SignedTransferV1Request = {

    SignedTransferV1Request(
      Base58.encode(tx.sender),
      tx.assetId.compatId.map(_.toString),
      tx.recipient.stringRepr,
      tx.amount,
      tx.fee,
      tx.feeAssetId.compatId.map(_.toString),
      tx.timestamp,
      tx.attachment.headOption.map(_ => Base58.encode(tx.attachment)),
      tx.signature.toString
    )
  }

  def to(endpoint: String) = new Node(endpoint)

  class Node(endpoint: String) {

    def get(path: String, f: RequestBuilder => RequestBuilder = identity)(implicit tag: String): Future[Response] =
      retrying(f(_get(s"$endpoint$path")).build())

    def post(url: String, f: RequestBuilder => RequestBuilder = identity)(implicit tag: String): Future[Response] =
      retrying(f(_post(url)).build())

    def post(path: String, body: String)(implicit tag: String): Future[Response] =
      post(s"$endpoint$path", (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(body))

    def postJson[A: Writes](path: String, body: A)(implicit tag: String): Future[Response] =
      post(path, stringify(toJson(body)))

    def matcherPost[A: Writes](path: String, body: A)(implicit tag: String): Future[Response] =
      post(s"$endpoint$path", (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))

    def matcherGet(path: String, f: RequestBuilder => RequestBuilder = identity, statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200)(
        implicit tag: String): Future[Response] =
      retrying(f(_get(s"$endpoint$path")).build(), statusCode = statusCode)

    def placeOrder(order: Order)(implicit tag: String): Future[MatcherResponse] =
      matcherPost("/matcher/orderbook", order.json()).as[MatcherResponse]

    def height(endpoint: String)(implicit tag: String): Future[Int] = get("/blocks/height").as[JsValue].map(v => (v \ "height").as[Int])

    def transactionInfo(txId: String)(implicit tag: String): Future[Transaction] = get(s"/transactions/info/$txId").as[Transaction]

    def balance(address: String)(implicit tag: String): Future[Balance] = get(s"/addresses/balance/$address").as[Balance]

    def assetBalance(address: String, asset: String)(implicit tag: String): Future[AssetBalance] =
      get(s"/assets/balance/$address/$asset").as[AssetBalance]

    def balance(address: String, asset: Asset)(implicit tag: String): Future[Long] = asset match {
      case Plix => to(endpoint).balance(address).map(_.balance)
      case _ => to(endpoint).assetBalance(address, asset.compatId.map(_.toString).get).map(_.balance)
    }

    def orderbookByPublicKey(publicKey: String, ts: Long, signature: ByteStr, f: RequestBuilder => RequestBuilder = identity)(
        implicit tag: String): Future[Seq[OrderbookHistory]] =
      retrying {
        _get(s"$endpoint/matcher/orderbook/$publicKey")
          .setHeader("Timestamp", ts)
          .setHeader("Signature", signature)
          .build()
      }.as[Seq[OrderbookHistory]]

    def parseAssetPair(assetPair: AssetPair): (String, String) = {
      val amountAsset = AssetPair.assetIdStr(assetPair.amountAsset)
      val priceAsset  = AssetPair.assetIdStr(assetPair.priceAsset)
      (amountAsset, priceAsset)
    }

    def orderBook(assetPair: AssetPair)(implicit tag: String): Future[OrderBookResponse] = {
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherGet(s"/matcher/orderbook/$amountAsset/$priceAsset").as[OrderBookResponse]
    }

    def orderStatus(orderId: String, assetPair: AssetPair)(implicit tag: String): Future[MatcherStatusResponse] = {
      val (amountAsset, priceAsset) = parseAssetPair(assetPair)
      matcherGet(s"/matcher/orderbook/$amountAsset/$priceAsset/$orderId")
        .as[MatcherStatusResponse]
    }

    def cancelOrder(amountAsset: String, priceAsset: String, request: CancelOrderRequest)(implicit tag: String): Future[MatcherStatusResponse] =
      matcherPost(s"/matcher/orderbook/$amountAsset/$priceAsset/cancel", request).as[MatcherStatusResponse]

    def broadcastRequest[A: Writes](req: A)(implicit tag: String): Future[Transaction] = postJson("/transactions/broadcast", req).as[Transaction]

    def orderHistory(pk: PrivateKey)(implicit tag: String): Future[Seq[OrderbookHistory]] = {
      val ts        = System.currentTimeMillis()
      val signature = ByteStr(crypto.sign(pk, pk ++ Longs.toByteArray(ts)))
      orderbookByPublicKey(Base58.encode(pk), ts, signature)
    }

    def utx(implicit tag: String): Future[Seq[Transaction]] = get(s"/transactions/unconfirmed").as[Seq[Transaction]]

    def unconfirmedTxInfo(txId: String)(implicit tag: String): Future[Transaction] = get(s"/transactions/unconfirmed/info/$txId").as[Transaction]

    def findTransactionInfo(txId: String)(implicit tag: String): Future[Option[Transaction]] = transactionInfo(txId).transform {
      case Success(tx)                                          => Success(Some(tx))
      case Failure(UnexpectedStatusCodeException(_, _, 404, _)) => Success(None)
      case Failure(ex)                                          => Failure(ex)
    }

    def ensureTxDoesntExist(txId: String)(implicit tag: String): Future[Unit] =
      utx
        .zip(findTransactionInfo(txId))
        .flatMap({
          case (utx, _) if utx.map(_.id).contains(txId) =>
            Future.failed(new IllegalStateException(s"[$tag] Tx $txId is in UTX"))
          case (_, txOpt) if txOpt.isDefined =>
            Future.failed(new IllegalStateException(s"[$tag] Tx $txId is in blockchain"))
          case _ =>
            Future.successful(())
        })

    def waitForTransaction(txId: String, retryInterval: FiniteDuration = 1.second)(implicit tag: String): Future[Transaction] =
      waitFor[Option[Transaction]](s"transaction $txId")(
        _.transactionInfo(txId)
          .map(x => Option(x))
          .recoverWith {
            case e: UnexpectedStatusCodeException if e.statusCode == 404 => Future.successful(None)
          },
        _.exists(_.id == txId),
        retryInterval
      ).map(_.get)

    def waitFor[A](desc: String)(f: this.type => Future[A], cond: A => Boolean, retryInterval: FiniteDuration)(implicit tag: String): Future[A] = {
      log.debug(s"[$tag] Awaiting condition '$desc'")
      timer
        .retryUntil(f(this), cond, retryInterval)
        .map(a => {
          log.debug(s"[$tag] Condition '$desc' met")
          a
        })
    }
  }

}
