package com.plixplatform.dex

import java.util.concurrent.atomic.AtomicLong

import com.google.common.base.Charsets
import com.google.common.primitives.{Bytes, Ints}
import com.typesafe.config.ConfigFactory
import com.plixplatform.account.KeyPair
import com.plixplatform.common.state.ByteStr
import com.plixplatform.dex.model.MatcherModel.Price
import com.plixplatform.dex.model.{BuyLimitOrder, LimitOrder, OrderValidator, SellLimitOrder}
import com.plixplatform.dex.queue.{QueueEvent, QueueEventWithMeta}
import com.plixplatform.dex.settings.OrderFeeSettings._
import com.plixplatform.dex.settings.{AssetType, MatcherSettings}
import com.plixplatform.settings.loadConfig
import com.plixplatform.transaction.Asset
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.assets.exchange.OrderOps._
import com.plixplatform.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV3}
import com.plixplatform.{NTPTime, crypto}
import net.ceedubs.ficus.Ficus._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Suite

import scala.util.Random

trait MatcherTestData extends NTPTime { _: Suite =>
  private val signatureSize = 32

  val bytes32gen: Gen[Array[Byte]] = Gen.listOfN(signatureSize, Arbitrary.arbitrary[Byte]).map(xs => xs.toArray)
  val WalletSeed                   = ByteStr("Matcher".getBytes("utf-8"))
  val MatcherSeed: Array[Byte]     = crypto.secureHash(Bytes.concat(Ints.toByteArray(0), WalletSeed.arr))
  val MatcherAccount               = KeyPair(MatcherSeed)
  val accountGen: Gen[KeyPair]     = bytes32gen.map(seed => KeyPair(seed))
  val positiveLongGen: Gen[Long]   = Gen.choose(1, Long.MaxValue)

  private val seqNr = new AtomicLong(-1)

  def rateCache: RateCache = RateCache.inMem

  def wrap(x: Order): QueueEventWithMeta                   = wrap(seqNr.incrementAndGet(), x)
  def wrap(n: Long, x: Order): QueueEventWithMeta          = wrap(n, QueueEvent.Placed(x))
  def wrap(n: Long, event: QueueEvent): QueueEventWithMeta = QueueEventWithMeta(n, System.currentTimeMillis(), event)

  def assetIdGen(prefix: Byte): Gen[IssuedAsset] =
    Gen
      .listOfN(signatureSize - 1, Arbitrary.arbitrary[Byte])
      .map(xs => IssuedAsset(ByteStr(Array(prefix, xs: _*))))

  def arbitraryAssetIdGen: Gen[IssuedAsset] = assetIdGen(Random.nextInt(Byte.MaxValue).toByte)

  val distinctPairGen: Gen[AssetPair] = for {
    a1 <- assetIdGen(1.toByte)
    a2 <- assetIdGen(2.toByte)
  } yield AssetPair(a1, a2)

  protected def mkAssetId(prefix: String): IssuedAsset = {
    val prefixBytes = prefix.getBytes(Charsets.UTF_8)
    IssuedAsset(ByteStr((prefixBytes ++ Array.fill[Byte](32 - prefixBytes.length)(0.toByte)).take(32)))
  }

  val assetPairGen = Gen.frequency((18, distinctPairGen), (1, assetIdGen(1).map(AssetPair(_, Plix))), (1, assetIdGen(2).map(AssetPair(Plix, _))))

  val maxTimeGen: Gen[Long]     = Gen.choose(10000L, Order.MaxLiveTime).map(_ + System.currentTimeMillis())
  val createdTimeGen: Gen[Long] = Gen.choose(0L, 10000L).map(System.currentTimeMillis() - _)

  val config = loadConfig(ConfigFactory.parseString("""plix {
      |  directory: "/tmp/plix-test"
      |  matcher {
      |    account: ""
      |    bind-address: "127.0.0.1"
      |    port: 6886
      |    order-history-file: null
      |    min-order-fee: 100000
      |    snapshots-interval: 100000
      |    max-open-orders: 1000
      |    price-assets: ["BASE1", "BASE2", "BASE"]
      |    blacklisted-assets: ["BLACKLST"]
      |    blacklisted-names: ["[Ff]orbidden"]
      |    allowed-order-versions = [1, 2, 3]
      |  }
      |}""".stripMargin))

  val matcherSettings = config.as[MatcherSettings]("plix.dex")

  def valueFromGen[T](gen: Gen[T]): T = {
    var value = gen.sample
    while (value.isEmpty) {
      value = gen.sample
    }
    value.get
  }

  val maxPlixAmountGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L)

  def buyGenerator(pair: AssetPair,
                   amount: Long,
                   price: Long,
                   sender: Option[KeyPair] = None,
                   matcherFee: Option[Long] = None,
                   version: Byte = 1,
                   timestamp: Option[Long]): Gen[(Order, KeyPair)] =
    for {
      sender: KeyPair  <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long  <- timestamp.map(Gen.const).getOrElse(createdTimeGen)
      expiration: Long <- maxTimeGen
      matcherFee: Long <- matcherFee.map(Gen.const).getOrElse(maxPlixAmountGen)
    } yield (Order.buy(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, version), sender)

  def sellGenerator(pair: AssetPair,
                    amount: Price,
                    price: Price,
                    sender: Option[KeyPair] = None,
                    matcherFee: Option[Price] = None,
                    timestamp: Option[Price],
                    version: Byte = 1): Gen[(Order, KeyPair)] =
    for {
      sender: KeyPair  <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long  <- timestamp.map(Gen.const).getOrElse(createdTimeGen)
      expiration: Long <- maxTimeGen
      matcherFee: Long <- matcherFee.map(Gen.const).getOrElse(maxPlixAmountGen)
    } yield (Order.sell(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, version), sender)

  def buy(pair: AssetPair,
          amount: Price,
          price: BigDecimal,
          sender: Option[KeyPair] = None,
          matcherFee: Option[Price] = None,
          ts: Option[Price] = None,
          version: Byte = 1): Order = rawBuy(pair, amount, (price * Order.PriceConstant).toLong, sender, matcherFee, ts, version)

  def rawBuy(pair: AssetPair,
             amount: Price,
             price: Price,
             sender: Option[KeyPair] = None,
             matcherFee: Option[Price] = None,
             ts: Option[Price] = None,
             version: Byte = 1): Order =
    valueFromGen(buyGenerator(pair, amount, price, sender, matcherFee, version, ts))._1

  def sell(pair: AssetPair,
           amount: Price,
           price: BigDecimal,
           sender: Option[KeyPair] = None,
           matcherFee: Option[Price] = None,
           ts: Option[Price] = None,
           version: Byte = 1): Order = rawSell(pair, amount, (price * Order.PriceConstant).toLong, sender, matcherFee, ts, version)

  def rawSell(pair: AssetPair,
              amount: Price,
              price: Price,
              sender: Option[KeyPair] = None,
              matcherFee: Option[Price] = None,
              ts: Option[Price] = None,
              version: Byte = 1): Order =
    valueFromGen(sellGenerator(pair, amount, price, sender, matcherFee, ts, version))._1

  val orderTypeGenerator: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  def orderGenerator(sender: KeyPair, pair: AssetPair): Gen[Order] =
    for {
      orderType          <- orderTypeGenerator
      amount: Long       <- maxPlixAmountGen
      price: Long        <- Gen.choose((BigDecimal(10).pow(8) / amount).toLong.max(1), (Long.MaxValue / amount) - 100)
      timestamp: Long    <- createdTimeGen
      expiration: Long   <- maxTimeGen
      matcherFee: Long   <- maxPlixAmountGen
      orderVersion: Byte <- Gen.oneOf(1: Byte, 2: Byte, 3: Byte)
      arbitraryAsset     <- arbitraryAssetIdGen
      matcherFeeAssetId  <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Plix, arbitraryAsset)
    } yield {
      if (orderVersion == 3)
        Order(sender, MatcherAccount, pair, orderType, amount, price, timestamp, expiration, matcherFee, orderVersion, matcherFeeAssetId)
      else Order(sender, MatcherAccount, pair, orderType, amount, price, timestamp, expiration, matcherFee, orderVersion)
    }

  val orderGenerator: Gen[(Order, KeyPair)] = for {
    sender: KeyPair <- accountGen
    pair            <- assetPairGen
    order           <- orderGenerator(sender, pair)
  } yield order -> sender

  val buyLimitOrderGenerator: Gen[BuyLimitOrder] = for {
    sender: KeyPair    <- accountGen
    pair               <- assetPairGen
    amount: Long       <- maxPlixAmountGen
    price: Long        <- maxPlixAmountGen
    timestamp: Long    <- createdTimeGen
    expiration: Long   <- maxTimeGen
    matcherFee: Long   <- maxPlixAmountGen
    orderVersion: Byte <- Gen.oneOf(1: Byte, 2: Byte)
  } yield BuyLimitOrder(amount, matcherFee, Order.buy(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, orderVersion))

  val sellLimitOrderGenerator: Gen[SellLimitOrder] = for {
    sender: KeyPair    <- accountGen
    pair               <- assetPairGen
    amount: Long       <- maxPlixAmountGen
    price: Long        <- maxPlixAmountGen
    timestamp: Long    <- createdTimeGen
    expiration: Long   <- maxTimeGen
    matcherFee: Long   <- maxPlixAmountGen
    orderVersion: Byte <- Gen.oneOf(1: Byte, 2: Byte)
  } yield SellLimitOrder(amount, matcherFee, Order.sell(sender, MatcherAccount, pair, amount, price, timestamp, expiration, matcherFee, orderVersion))

  val limitOrderGenerator: Gen[LimitOrder] = Gen.oneOf(sellLimitOrderGenerator, buyLimitOrderGenerator)

  val orderV3Generator: Gen[Order] =
    for {
      sender: KeyPair   <- accountGen
      pair              <- assetPairGen
      orderType         <- orderTypeGenerator
      amount: Long      <- maxPlixAmountGen
      price: Long       <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestamp: Long   <- createdTimeGen
      expiration: Long  <- maxTimeGen
      matcherFee: Long  <- maxPlixAmountGen
      arbitraryAsset    <- arbitraryAssetIdGen
      matcherFeeAssetId <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Plix, arbitraryAsset)
    } yield {
      OrderV3(sender, MatcherAccount, pair, orderType, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId)
    }

  def orderV3WithPredefinedFeeAssetGenerator(matcherFeeAsset: Option[Asset] = None): Gen[(KeyPair, Order)] = {
    for {
      sender: KeyPair  <- accountGen
      pair             <- distinctPairGen
      orderType        <- orderTypeGenerator
      amount: Long     <- maxPlixAmountGen
      price: Long      <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestamp: Long  <- createdTimeGen
      expiration: Long <- maxTimeGen
      matcherFee: Long <- maxPlixAmountGen
      arbitraryAsset   <- arbitraryAssetIdGen
    } yield {

      sender -> OrderV3(sender,
                        MatcherAccount,
                        pair,
                        orderType,
                        amount,
                        price,
                        timestamp,
                        expiration,
                        matcherFee,
                        matcherFeeAsset getOrElse arbitraryAsset)
    }
  }

  val orderV3PairGenerator: Gen[((KeyPair, Order), (KeyPair, Order))] =
    for {
      senderBuy: KeyPair   <- accountGen
      senderSell: KeyPair  <- accountGen
      pair                 <- assetPairGen
      amount: Long         <- maxPlixAmountGen
      price: Long          <- Gen.choose(1, (Long.MaxValue / amount) - 100)
      timestampBuy: Long   <- createdTimeGen
      timestampSell: Long  <- createdTimeGen
      expirationBuy: Long  <- maxTimeGen
      expirationSell: Long <- maxTimeGen
      matcherFeeBuy: Long  <- maxPlixAmountGen
      matcherFeeSell: Long <- maxPlixAmountGen
      arbitraryAsset       <- arbitraryAssetIdGen
      matcherFeeAssetId    <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Plix, arbitraryAsset)
    } yield {
      (
        senderBuy -> OrderV3(senderBuy,
                             MatcherAccount,
                             pair,
                             OrderType.BUY,
                             Order.correctAmount(amount, price),
                             price,
                             timestampBuy,
                             expirationBuy,
                             matcherFeeBuy,
                             matcherFeeAssetId),
        senderSell -> OrderV3(senderSell,
                              MatcherAccount,
                              pair,
                              OrderType.SELL,
                              Order.correctAmount(amount, price),
                              price,
                              timestampSell,
                              expirationSell,
                              matcherFeeSell,
                              matcherFeeAssetId)
      )
    }

  val percentSettingsGenerator: Gen[PercentSettings] =
    for {
      assetType <- Gen.oneOf(AssetType.values.toSeq)
      minFee    <- Gen.choose(0.01, 100.0)
    } yield PercentSettings(assetType, minFee)

  def fixedSettingsGenerator(defaultAsset: Asset, lowerMinFeeBound: Long = 1, upperMinFeeBound: Long = 1000000L): Gen[FixedSettings] =
    for { minFee <- Gen.choose(lowerMinFeeBound, upperMinFeeBound) } yield { FixedSettings(defaultAsset, minFee) }

  def dynamicSettingsGenerator(lowerBaseFeeBound: Long = 1, upperBaseFeeBound: Long = 1000000L): Gen[DynamicSettings] =
    for { baseFee <- Gen.choose(lowerBaseFeeBound, upperBaseFeeBound) } yield { DynamicSettings(baseFee) }

  def orderFeeSettingsGenerator(defaultAssetForFixedSettings: Option[Asset] = None): Gen[OrderFeeSettings] = {
    for {
      defaultAsset     <- defaultAssetForFixedSettings.fold(arbitraryAssetIdGen)(_.fold(arbitraryAssetIdGen)(Gen.const))
      orderFeeSettings <- Gen.oneOf(dynamicSettingsGenerator(), fixedSettingsGenerator(defaultAsset), percentSettingsGenerator)
    } yield orderFeeSettings
  }

  val orderWithFeeSettingsGenerator: Gen[(Order, KeyPair, OrderFeeSettings)] = {
    for {
      orderFeeSettings <- orderFeeSettingsGenerator()
      (order, sender)  <- orderGenerator
    } yield {
      val correctedOrder = correctOrderByFeeSettings(order, sender, orderFeeSettings)
      (correctedOrder, sender, orderFeeSettings)
    }
  }

  def orderWithFeeSettingsGenerator(tpe: OrderType, price: Long): Gen[(Order, OrderFeeSettings)] =
    for {
      sender: KeyPair    <- accountGen
      pair               <- assetPairGen
      amount: Long       <- maxPlixAmountGen
      timestamp: Long    <- createdTimeGen
      expiration: Long   <- maxTimeGen
      matcherFee: Long   <- maxPlixAmountGen
      orderVersion: Byte <- Gen.oneOf(1: Byte, 2: Byte, 3: Byte)
      arbitraryAsset     <- arbitraryAssetIdGen
      matcherFeeAssetId  <- Gen.oneOf(pair.amountAsset, pair.priceAsset, Plix, arbitraryAsset)
      orderFeeSettings   <- orderFeeSettingsGenerator(Some(arbitraryAsset))
    } yield {

      val order =
        if (orderVersion == 3)
          Order(sender, MatcherAccount, pair, tpe, amount, price, timestamp, expiration, matcherFee, orderVersion, matcherFeeAssetId)
        else Order(sender, MatcherAccount, pair, tpe, amount, price, timestamp, expiration, matcherFee, orderVersion)

      val correctedOrder = correctOrderByFeeSettings(order, sender, orderFeeSettings)

      correctedOrder -> orderFeeSettings
    }

  val orderWithoutPlixInPairAndWithFeeSettingsGenerator: Gen[(Order, KeyPair, OrderFeeSettings)] = {
    for {
      sender: KeyPair  <- accountGen
      amountAsset      <- arbitraryAssetIdGen
      priceAsset       <- arbitraryAssetIdGen
      orderFeeSettings <- orderFeeSettingsGenerator()
      order            <- orderGenerator(sender, AssetPair(amountAsset, priceAsset))
    } yield {
      val correctedOrder = correctOrderByFeeSettings(order, sender, orderFeeSettings)
      (correctedOrder, sender, orderFeeSettings)
    }
  }

  val orderV3WithFeeSettingsGenerator: Gen[(Order, OrderFeeSettings)] = {
    for {
      (sender, order)  <- orderV3WithPredefinedFeeAssetGenerator()
      orderFeeSettings <- orderFeeSettingsGenerator(Some(order.matcherFeeAssetId))
    } yield {
      correctOrderByFeeSettings(order, sender, orderFeeSettings) -> orderFeeSettings
    }
  }

  val orderV3WithDynamicFeeSettingsAndRateCacheGen: Gen[(Order, DynamicSettings, RateCache)] = {
    for {
      (sender, order) <- orderV3WithPredefinedFeeAssetGenerator()
      dynamicSettings <- dynamicSettingsGenerator()
      rate            <- Gen.choose[Double](0, 10).map(_ / 10)
    } yield {
      val ratesMap = rateCache
      ratesMap.upsertRate(order.matcherFeeAssetId, rate)
      (correctOrderByFeeSettings(order, sender, dynamicSettings, Some(order.matcherFeeAssetId), Some(rate)), dynamicSettings, ratesMap)
    }
  }

  private def correctOrderByFeeSettings(order: Order,
                                        sender: KeyPair,
                                        orderFeeSettings: OrderFeeSettings,
                                        matcherFeeAssetForDynamicSettings: Option[Asset] = None,
                                        rateForDynamicSettings: Option[Double] = None): Order = {
    val correctedOrder = (order.version, orderFeeSettings) match {
      case (3, FixedSettings(defaultAssetId, minFee)) =>
        order
          .updateMatcherFeeAssetId(defaultAssetId)
          .updateFee(minFee)
      case (3, percentSettings: PercentSettings) =>
        order
          .updateMatcherFeeAssetId(OrderValidator.getValidFeeAssetForSettings(order, percentSettings, rateCache).head)
          .updateFee(OrderValidator.getMinValidFeeForSettings(order, percentSettings, order.price, rateCache))
      case (_, DynamicSettings(baseFee)) =>
        order
          .updateMatcherFeeAssetId(matcherFeeAssetForDynamicSettings getOrElse Plix)
          .updateFee(
            rateForDynamicSettings.fold(baseFee) { rate =>
              OrderValidator.multiplyFeeByDouble(baseFee, rate)
            }
          )
      case _ => order
    }

    Order.sign(correctedOrder, sender)
  }
}
