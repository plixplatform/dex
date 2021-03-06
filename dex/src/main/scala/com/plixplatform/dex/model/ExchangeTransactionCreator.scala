package com.plixplatform.dex.model

import com.plixplatform.account.{Address, KeyPair}
import com.plixplatform.common.utils.EitherExt2
import com.plixplatform.features.BlockchainFeatures
import com.plixplatform.features.FeatureProvider.FeatureProviderExt
import com.plixplatform.lang.ValidationError
import com.plixplatform.dex.model.ExchangeTransactionCreator._
import com.plixplatform.dex.settings.AssetType.AssetType
import com.plixplatform.dex.settings.OrderFeeSettings.PercentSettings
import com.plixplatform.dex.settings.{AssetType, MatcherSettings}
import com.plixplatform.state.Blockchain
import com.plixplatform.state.diffs.FeeValidation
import com.plixplatform.transaction.Asset
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.TxValidationError._
import com.plixplatform.transaction.assets.exchange._

class ExchangeTransactionCreator(blockchain: Blockchain, matcherPrivateKey: KeyPair, matcherSettings: MatcherSettings) {

  private def calculateMatcherFee(buy: Order, sell: Order, executedAmount: Long, executedPrice: Long): (Long, Long) = {

    def calcFee(o: Order, executedAmount: Long, totalAmount: Long): Long = {
      val p = BigInt(executedAmount) * o.matcherFee / totalAmount
      p.toLong
    }

    def getActualBuySellAmounts(assetType: AssetType, buyAmount: Long, buyPrice: Long, sellAmount: Long, sellPrice: Long): (Long, Long) = {

      val (buyAmt, sellAmt) = assetType match {
        case AssetType.AMOUNT    => buy.getReceiveAmount _ -> sell.getSpendAmount _
        case AssetType.PRICE     => buy.getSpendAmount _   -> sell.getReceiveAmount _
        case AssetType.RECEIVING => buy.getReceiveAmount _ -> sell.getReceiveAmount _
        case AssetType.SPENDING  => buy.getSpendAmount _   -> sell.getSpendAmount _
      }

      buyAmt(buyAmount, buyPrice).explicitGet() -> sellAmt(sellAmount, sellPrice).explicitGet()
    }

    matcherSettings.orderFee match {
      case PercentSettings(assetType, _) =>
        val (buyAmountExecuted, sellAmountExecuted) = getActualBuySellAmounts(assetType, executedAmount, executedPrice, executedAmount, executedPrice)
        val (buyAmountTotal, sellAmountTotal)       = getActualBuySellAmounts(assetType, buy.amount, buy.price, sell.amount, sell.price)

        (
          Math.min(buy.matcherFee, calcFee(buy, buyAmountExecuted, buyAmountTotal)),
          Math.min(sell.matcherFee, calcFee(sell, sellAmountExecuted, sellAmountTotal))
        )

      case _ => calcFee(buy, executedAmount, buy.amount) -> calcFee(sell, executedAmount, sell.amount)
    }
  }

  def createTransaction(submitted: LimitOrder, counter: LimitOrder, timestamp: Long): Either[ValidationError, ExchangeTransaction] = {

    val executedAmount    = LimitOrder.executedAmount(submitted, counter)
    val price             = counter.price
    val (buy, sell)       = Order.splitByType(submitted.order, counter.order)
    val (buyFee, sellFee) = calculateMatcherFee(buy, sell, executedAmount, price)

    val txFee = minFee(blockchain, matcherPrivateKey, counter.order.assetPair, matcherSettings.exchangeTxBaseFee)

    if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height))
      ExchangeTransactionV2.create(matcherPrivateKey, buy, sell, executedAmount, price, buyFee, sellFee, txFee, timestamp)
    else
      for {
        buyV1  <- toV1(buy)
        sellV1 <- toV1(sell)
        tx     <- ExchangeTransactionV1.create(matcherPrivateKey, buyV1, sellV1, executedAmount, price, buyFee, sellFee, txFee, timestamp)
      } yield tx
  }

  private def toV1(order: Order): Either[ValidationError, OrderV1] = order match {
    case x: OrderV1 => Right(x)
    case _          => Left(ActivationError("Smart Account Trading feature has not been activated yet"))
  }
}

object ExchangeTransactionCreator {

  type CreateTransaction = (LimitOrder, LimitOrder, Long) => Either[ValidationError, ExchangeTransaction]

  /**
    * This function is used for the following purposes:
    *
    *   1. Calculate transaction fee that matcher pays to issue Exchange transaction (ExchangeTransactionCreator, base fee = matcherSettings.exchangeTxBaseFee)
    *   2. Calculate matcher fee that client pays for the order placement and covering matcher expenses (OrderValidator blockchain aware, base fee depends on order fee settings)
    *
    * @see [[com.plixplatform.transaction.smart.Verifier#verifyExchange verifyExchange]]
    */
  def minFee(blockchain: Blockchain, matcherAddress: Address, assetPair: AssetPair, baseFee: Long): Long = {

    def assetFee(assetId: Asset): Long = assetId match {
      case Plix => 0L
      case asset: IssuedAsset =>
        if (blockchain hasAssetScript asset) FeeValidation.ScriptExtraFee
        else 0L
    }

    baseFee +
      minAccountFee(blockchain, matcherAddress) +
      assetPair.amountAsset.fold(0L)(assetFee) +
      assetPair.priceAsset.fold(0L)(assetFee)
  }

  def minAccountFee(blockchain: Blockchain, address: Address): Long = {
    if (blockchain hasScript address) FeeValidation.ScriptExtraFee else 0L
  }
}
