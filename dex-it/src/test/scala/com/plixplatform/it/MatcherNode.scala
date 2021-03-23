package com.plixplatform.it

import com.plixplatform.account.KeyPair
import com.plixplatform.common.utils.EitherExt2
import com.plixplatform.it.api.SyncHttpApi._
import com.plixplatform.it.util._
import com.plixplatform.transaction.smart.SetScriptTransaction
import com.plixplatform.transaction.smart.script.ScriptCompiler
import com.plixplatform.utils.ScorexLogging
import org.scalatest.{BeforeAndAfterAll, Suite}

trait MatcherNode extends BeforeAndAfterAll with Nodes with ScorexLogging {
  this: Suite =>

  def setContract(contractText: Option[String], acc: KeyPair): String = {
    val script = contractText.map { x =>
      val scriptText = x.stripMargin
      ScriptCompiler(scriptText, isAssetScript = false).explicitGet()._1
    }
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(acc, script, 0.014.plix, System.currentTimeMillis())
      .explicitGet()

    nodes.head
      .signedBroadcast(setScriptTransaction.json(), waitForTx = true)
      .id
  }
}
