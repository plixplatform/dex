package com.plixplatform.it.sync.config

import java.nio.charset.StandardCharsets

import com.typesafe.config.ConfigFactory.parseString
import com.typesafe.config.{Config, ConfigFactory}
import com.plixplatform.account.{AddressScheme, KeyPair}
import com.plixplatform.common.utils.EitherExt2
import com.plixplatform.it.sync.{issueFee, someAssetAmount}
import com.plixplatform.it.util._
import com.plixplatform.dex.AssetPairBuilder
import com.plixplatform.dex.market.MatcherActor
import com.plixplatform.transaction.Asset
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.assets.exchange.AssetPair
import com.plixplatform.transaction.assets.{IssueTransaction, IssueTransactionV1, IssueTransactionV2}
import com.plixplatform.wallet.Wallet

import scala.collection.JavaConverters._
import scala.util.Random

object MatcherPriceAssetConfig {

  private val genesisConfig = ConfigFactory.parseResources("genesis.conf")
  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = genesisConfig.getString("genesis-generator.network-type").head.toByte
  }

  val accounts: Map[String, KeyPair] = {
    val config           = ConfigFactory.parseResources("genesis.conf")
    val distributionsKey = "genesis-generator.distributions"
    val distributions    = config.getObject(distributionsKey)
    distributions
      .keySet()
      .asScala
      .map { accountName =>
        val prefix   = s"$distributionsKey.$accountName"
        val seedText = config.getString(s"$prefix.seed-text")
        val nonce    = config.getInt(s"$prefix.nonce")
        accountName -> Wallet.generateNewAccount(seedText.getBytes(StandardCharsets.UTF_8), nonce)
      }
      .toMap
  }

  val matcher: KeyPair = accounts("matcher")
  val alice: KeyPair   = accounts("alice")
  val bob: KeyPair     = accounts("bob")

  val Decimals: Byte = 2

  val usdAssetName = "USD-X"
  val wctAssetName = "WCT-X"
  val ethAssetName = "ETH-X"
  val btcAssetName = "BTC-X"

  val defaultAssetQuantity = 999999999999L

  val IssueUsdTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = alice,
      name = usdAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      script = None,
      fee = 1.plix,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueWctTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = bob,
      name = wctAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      script = None,
      fee = 1.plix,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueEthTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = alice,
      name = ethAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      script = None,
      fee = 1.plix,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueBtcTx: IssueTransactionV2 = IssueTransactionV2
    .selfSigned(
      AddressScheme.current.chainId,
      sender = bob,
      name = btcAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      script = None,
      fee = 1.plix,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val BtcId = IssueBtcTx.id()
  val EthId = IssueEthTx.id()
  val UsdId = IssueUsdTx.id()
  val WctId = IssueWctTx.id()

  val wctUsdPair = AssetPair(
    amountAsset = IssuedAsset(WctId),
    priceAsset = IssuedAsset(UsdId)
  )

  val wctPlixPair = AssetPair(
    amountAsset = IssuedAsset(WctId),
    priceAsset = Plix
  )

  val ethPlixPair = AssetPair(
    amountAsset = IssuedAsset(EthId),
    priceAsset = Plix
  )

  val ethBtcPair = AssetPair(
    amountAsset = IssuedAsset(EthId),
    priceAsset = IssuedAsset(BtcId)
  )

  val plixUsdPair = AssetPair(
    amountAsset = Plix,
    priceAsset = IssuedAsset(UsdId)
  )

  val ethUsdPair = AssetPair(
    amountAsset = IssuedAsset(EthId),
    priceAsset = IssuedAsset(UsdId)
  )

  val plixBtcPair = AssetPair(
    amountAsset = Plix,
    priceAsset = IssuedAsset(BtcId)
  )

  val orderLimit = 10

  val ForbiddenAssetId     = "FdbnAsset"
  val updatedMatcherConfig = parseString(s"""plix.dex {
                                            |  blacklisted-assets = ["$ForbiddenAssetId"]
                                            |  price-assets = [ "$UsdId", "$BtcId", "PLIX" ]
                                            |  rest-order-limit = $orderLimit
                                            |  snapshots-interval = 10
                                            |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    updatedMatcherConfig.withFallback(ConfigFactory.parseResources("nodes.conf").getConfigList("nodes").asScala.head)
  )

  def createAssetPair(asset1: String, asset2: String): AssetPair = {
    val (a1, a2) = (AssetPair.extractAssetId(asset1).get, AssetPair.extractAssetId(asset2).get)
    if (AssetPairBuilder.assetIdOrdering.compare(a1.compatId, a2.compatId) > 0)
      AssetPair(a1, a2)
    else
      AssetPair(a2, a1)
  }

  def issueAssetPair(issuer: KeyPair, amountAssetDecimals: Byte, priceAssetDecimals: Byte): (IssueTransaction, IssueTransaction, AssetPair) = {
    issueAssetPair(issuer, issuer, amountAssetDecimals, priceAssetDecimals)
  }

  def issueAssetPair(amountAssetIssuer: KeyPair,
                     priceAssetIssuer: KeyPair,
                     amountAssetDecimals: Byte,
                     priceAssetDecimals: Byte): (IssueTransaction, IssueTransaction, AssetPair) = {
    val issueAmountAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = amountAssetIssuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = amountAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .explicitGet()

    val issuePriceAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = priceAssetIssuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = priceAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .explicitGet()

    if (MatcherActor.compare(Some(issuePriceAssetTx.id().arr), Some(issueAmountAssetTx.id().arr)) < 0) {
      (issueAmountAssetTx,
       issuePriceAssetTx,
       AssetPair(
         amountAsset = IssuedAsset(issueAmountAssetTx.id()),
         priceAsset = IssuedAsset(issuePriceAssetTx.id())
       ))
    } else
      issueAssetPair(amountAssetIssuer, priceAssetIssuer, amountAssetDecimals, priceAssetDecimals)
  }

  def assetPairIssuePriceAsset(issuer: KeyPair, amountAssetId: Asset, priceAssetDecimals: Byte): (IssueTransaction, AssetPair) = {
    val issuePriceAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = issuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = priceAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get

    if (MatcherActor.compare(Some(issuePriceAssetTx.id().arr), amountAssetId.compatId.map(_.arr)) < 0) {
      (issuePriceAssetTx,
       AssetPair(
         amountAsset = amountAssetId,
         priceAsset = IssuedAsset(issuePriceAssetTx.id())
       ))
    } else
      assetPairIssuePriceAsset(issuer, amountAssetId, priceAssetDecimals)
  }

}
