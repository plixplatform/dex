package com.plixplatform.dex.smart

import com.plixplatform.account.{KeyPair, PublicKey}
import com.plixplatform.common.state.ByteStr
import com.plixplatform.common.utils.EitherExt2
import com.plixplatform.dex.error.ProduceError.produce
import com.plixplatform.lang.script.Script
import com.plixplatform.lang.v1.compiler.Terms
import com.plixplatform.lang.v1.evaluator.Log
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.assets.exchange.{AssetPair, OrderType, OrderV1}
import com.plixplatform.transaction.smart.script.ScriptCompiler
import com.plixplatform.{NoShrink, TransactionGen}
import org.scalatest.{FreeSpecLike, Matchers}

import scala.util.Try

class MatcherScriptRunnerTest extends FreeSpecLike with Matchers with TransactionGen with NoShrink {
  private val sampleOrder = OrderV1(
    sender = KeyPair("test".getBytes()),
    matcher = PublicKey("matcher".getBytes("utf-8")),
    pair = AssetPair(Plix, IssuedAsset(ByteStr("asset".getBytes("utf-8")))),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = 1L,
    expiration = 1000L,
    matcherFee = 30000L
  )

  private def run(script: Script): (Log, Either[String, Terms.EVALUATED]) = MatcherScriptRunner(script, sampleOrder)

  "dApp sunny day" in {
    run(dAppScriptSunny)._2.explicitGet() shouldBe Terms.FALSE
  }

  "Blockchain functions are disabled in dApp" in {
    Try(run(dAppScriptBlockchain)).toEither should produce("""An access to the blockchain\.accountData is denied on DEX""".r)
  }

  private def dAppScriptSunny: Script =
    ScriptCompiler
      .compile(
        s"""|{-# STDLIB_VERSION 3 #-}
            |{-# CONTENT_TYPE DAPP #-}
            |{-# SCRIPT_TYPE ACCOUNT #-}
            |
            |let addr = addressFromPublicKey(base58'H1kGVNdJwV7N5dF73YWs1R8uet6g5bCvTHmTnYy1hSnr')
            |
            |@Verifier(x)
            |func verifier() = {
            |    match(x) {
            |      case o: Order => o.sender == addr
            |      case _ => false
            |    }
            |}
            |""".stripMargin
      )
      .explicitGet()
      ._1

  private def dAppScriptBlockchain: Script =
    ScriptCompiler
      .compile(
        s"""|{-# STDLIB_VERSION 3 #-}
            |{-# CONTENT_TYPE DAPP #-}
            |{-# SCRIPT_TYPE ACCOUNT #-}
            |
            |@Verifier(x)
            |func verifier() = {
            |    match(x) {
            |      case o: Order => getBooleanValue(o.sender, "foo")
            |      case _ => false
            |    }
            |}
            |""".stripMargin
      )
      .explicitGet()
      ._1
}
