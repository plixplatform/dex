package com.plixplatform

import com.plixplatform.account.{KeyPair, PublicKey}
import com.plixplatform.it.api.AsyncMatcherHttpApi._
import com.plixplatform.it.api.MatcherCommand
import com.plixplatform.it.sync.matcherFee
import com.plixplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalacheck.Gen

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.util.control.NonFatal

package object it {
  def executeCommands(xs: Seq[MatcherCommand], ignoreErrors: Boolean = true, timeout: FiniteDuration = 3.minutes): Unit =
    Await.ready(Future.sequence(xs.map(executeCommand(_))), timeout)

  private def executeCommand(x: MatcherCommand, ignoreErrors: Boolean = true): Future[Unit] = x match {
    case MatcherCommand.Place(node, order) => node.placeOrder(order).map(_ => ())
    case MatcherCommand.Cancel(node, owner, order) =>
      try node.cancelOrder(owner, order.assetPair, order.idStr()).map(_ => ())
      catch {
        case NonFatal(e) =>
          if (ignoreErrors) Future.successful(())
          else Future.failed(e)
      }
  }

  def orderGen(matcher: PublicKey, trader: KeyPair, assetPairs: Seq[AssetPair]): Gen[Order] =
    for {
      assetPair      <- Gen.oneOf(assetPairs)
      tpe            <- Gen.oneOf(OrderType.BUY, OrderType.SELL)
      amount         <- Gen.choose(10, 100)
      price          <- Gen.choose(10, 100)
      orderVersion   <- Gen.oneOf(1: Byte, 2: Byte)
      expirationDiff <- Gen.choose(600000, 6000000)
    } yield {
      val ts = System.currentTimeMillis()
      if (tpe == OrderType.BUY)
        Order.buy(
          trader,
          matcher,
          assetPair,
          amount,
          price * Order.PriceConstant,
          System.currentTimeMillis(),
          ts + expirationDiff,
          matcherFee,
          orderVersion
        )
      else
        Order.sell(
          trader,
          matcher,
          assetPair,
          amount,
          price * Order.PriceConstant,
          System.currentTimeMillis(),
          ts + expirationDiff,
          matcherFee,
          orderVersion
        )
    }

  def choose[T](xs: IndexedSeq[T]): T = xs(Random.nextInt(xs.size))
}
