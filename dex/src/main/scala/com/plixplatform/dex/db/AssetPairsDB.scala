package com.plixplatform.dex.db

import com.plixplatform.database.DBExt
import com.plixplatform.dex.MatcherKeys
import com.plixplatform.transaction.assets.exchange.AssetPair
import org.iq80.leveldb.DB

trait AssetPairsDB {
  def add(pair: AssetPair): Unit
  def remove(pair: AssetPair): Unit
  def all(): Set[AssetPair]
}

object AssetPairsDB {
  def apply(db: DB): AssetPairsDB = new AssetPairsDB {
    override def add(pair: AssetPair): Unit    = db.readWrite(_.put(MatcherKeys.assetPair(pair), ()))
    override def remove(pair: AssetPair): Unit = db.readWrite(_.delete(MatcherKeys.assetPair(pair)))
    override def all(): Set[AssetPair] = db.readOnly { ro =>
      val r = Set.newBuilder[AssetPair]
      ro.iterateOver(MatcherKeys.AssetPairsPrefix) { pair =>
        r += AssetPair.fromBytes(pair.getKey.drop(2))
      }
      r.result()
    }
  }
}
