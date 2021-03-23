package com.plixplatform.it.api

import com.plixplatform.account.KeyPair
import com.plixplatform.dex.queue.QueueEventWithMeta
import com.plixplatform.transaction.assets.exchange.AssetPair

case class MatcherState(offset: QueueEventWithMeta.Offset,
                        snapshots: Map[String, QueueEventWithMeta.Offset],
                        orderBooks: Map[AssetPair, (OrderBookResponse, MarketStatusResponse)],
                        orderStatuses: Map[String, MatcherStatusResponse],
                        reservedBalances: Map[KeyPair, Map[String, Long]],
                        orderHistory: Map[KeyPair, Map[AssetPair, Seq[OrderbookHistory]]])
