package com.plixplatform.it.api

import com.plixplatform.account.KeyPair
import com.plixplatform.it.Node
import com.plixplatform.transaction.assets.exchange.Order

sealed trait MatcherCommand extends Product with Serializable
object MatcherCommand {
  case class Place(node: Node, order: Order)                         extends MatcherCommand
  case class Cancel(node: Node, owner: KeyPair, order: Order) extends MatcherCommand
}
