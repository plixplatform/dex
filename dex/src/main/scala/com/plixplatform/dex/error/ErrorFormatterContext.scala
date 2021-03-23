package com.plixplatform.dex.error

import com.plixplatform.transaction.Asset

trait ErrorFormatterContext {
  def assetDecimals(asset: Asset): Int
}
