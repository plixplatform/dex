package com.plixplatform.dex.smart

import cats.Eval
import cats.implicits._
import com.plixplatform.account.AddressScheme
import com.plixplatform.lang.contract.DApp
import com.plixplatform.lang.script.v1.ExprScript
import com.plixplatform.lang.script.{ContractScript, Script}
import com.plixplatform.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.plixplatform.lang.v1.evaluator.{ContractEvaluator, EvaluatorV1, Log}
import com.plixplatform.transaction.assets.exchange.Order
import com.plixplatform.transaction.smart.{RealTransactionWrapper, Verifier}
import com.plixplatform.transaction.{Authorized, Proven}

object MatcherScriptRunner {

  def apply(script: Script, order: Order): (Log, Either[String, EVALUATED]) = script match {
    case s: ExprScript =>
      MatcherContext.build(script.stdLibVersion, AddressScheme.current.chainId, Eval.later(order), isDApp = false) match {
        case Left(error) => (List.empty, Left(error))
        case Right(ctx)  => EvaluatorV1.applyWithLogging(ctx, s.expr)
      }

    case ContractScript.ContractScriptImpl(_, DApp(_, decls, _, Some(vf)), _) =>
      MatcherContext.build(
        script.stdLibVersion,
        AddressScheme.current.chainId,
        Eval.later(order),
        isDApp = true
      ) match {
        case Left(error) => (List.empty, Left(error))
        case Right(ctx) =>
          val evalContract = ContractEvaluator.verify(decls, vf, RealTransactionWrapper.ord(order))
          EvaluatorV1.evalWithLogging(ctx, evalContract)
      }

    case ContractScript.ContractScriptImpl(_, DApp(_, _, _, None), _) =>
      (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](order) match {
        case Right(_) => Right(TRUE)
        case Left(_)  => Right(FALSE)
      })

    case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
  }
}
