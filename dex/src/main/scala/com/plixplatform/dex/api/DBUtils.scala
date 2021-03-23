package com.plixplatform.dex.api

import com.plixplatform.common.state.ByteStr
import com.plixplatform.database.DBExt
import com.plixplatform.dex._
import com.plixplatform.dex.model.OrderInfo.FinalOrderInfo
import com.plixplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import org.iq80.leveldb.DB

object DBUtils {
  def order(db: DB, orderId: ByteStr): Option[Order]              = db.get(MatcherKeys.order(orderId))
  def orderInfo(db: DB, orderId: ByteStr): Option[FinalOrderInfo] = db.get(MatcherKeys.orderInfo(orderId))

  def transactionsForOrder(db: DB, orderId: ByteStr): Seq[ExchangeTransaction] = db.readOnly { ro =>
    for {
      seqNr <- 1 to ro.get(MatcherKeys.orderTxIdsSeqNr(orderId))
      txId = ro.get(MatcherKeys.orderTxId(orderId, seqNr))
      tx <- ro.get(MatcherKeys.exchangeTransaction(txId))
    } yield tx
  }
}
