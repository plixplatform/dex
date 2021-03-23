package com.plixplatform.dex

package object util {
  def getSimpleName(x: Any): String = x.getClass.getSimpleName.replaceAll("\\$", "")
}
