package com.plixplatform.dex.api
import com.plixplatform.account.Address
import com.plixplatform.transaction.assets.exchange.AssetPair

case class BatchCancel(address: Address, assetPair: Option[AssetPair], timestamp: Long)
