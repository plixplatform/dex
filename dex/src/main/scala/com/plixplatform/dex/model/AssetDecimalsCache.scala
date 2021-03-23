package com.plixplatform.dex.model

import java.util.concurrent.ConcurrentHashMap

import com.plixplatform.state.Blockchain
import com.plixplatform.transaction.Asset
import com.plixplatform.transaction.assets.exchange.AssetPair
import com.plixplatform.utils.ScorexLogging

class AssetDecimalsCache(blockchain: Blockchain) extends ScorexLogging {

  private val PlixDecimals      = 8
  private val assetDecimalsCache = new ConcurrentHashMap[Asset, Int](1000, 0.9f, 10)

  def get(asset: Asset): Int = {
    asset.fold { PlixDecimals } { issuedAsset =>
      Option(assetDecimalsCache.get(asset)) getOrElse {
        val assetDecimals =
          blockchain
            .assetDescription(issuedAsset)
            .map(_.decimals)
            .getOrElse {
              log.error(s"Can not get asset decimals since asset '${AssetPair.assetIdStr(asset)}' not found!")
              8
            }

        assetDecimalsCache.put(issuedAsset, assetDecimals)
        assetDecimals
      }
    }
  }
}
