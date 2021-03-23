package com.plixplatform.state

import com.plixplatform.common.state.diffs.ProduceError
import com.plixplatform.db.WithState
import org.scalatest.Matchers

package object diffs extends WithState with Matchers {
  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)
}
