package com.plixplatform.dex.api
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.{DeserializationContext, JsonNode, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.plixplatform.common.state.ByteStr
import com.plixplatform.transaction.Asset.{IssuedAsset, Plix}
import com.plixplatform.transaction.assets.exchange.AssetPair

object JsonSerializer {

  private val coreTypeSerializers = new SimpleModule()
  coreTypeSerializers.addDeserializer(classOf[AssetPair], new AssetPairDeserializer)

  private val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)
  mapper.registerModule(coreTypeSerializers)

  def serialize(value: Any): String                             = mapper.writeValueAsString(value)
  def deserialize[T](value: String)(implicit m: Manifest[T]): T = mapper.readValue(value)

  private class AssetPairDeserializer extends StdDeserializer[AssetPair](classOf[AssetPair]) {
    override def deserialize(p: JsonParser, ctxt: DeserializationContext): AssetPair = {
      val node = p.getCodec.readTree[JsonNode](p)
      def readAssetId(fieldName: String) = {
        val x = node.get(fieldName).asText(AssetPair.PlixName)
        if (x == AssetPair.PlixName) Plix else IssuedAsset(ByteStr.decodeBase58(x).get)
      }

      AssetPair(readAssetId("amountAsset"), readAssetId("priceAsset"))
    }
  }

}
