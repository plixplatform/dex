package com.plixplatform.dex.api.http

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.github.swagger.akka.SwaggerHttpService
import com.github.swagger.akka.model.{Info, License}
import com.plixplatform.dex.Version
import com.plixplatform.settings.RestAPISettings
import io.swagger.models.{Scheme, Swagger}

class SwaggerDocService(val actorSystem: ActorSystem, val materializer: ActorMaterializer, val apiClasses: Set[Class[_]], settings: RestAPISettings)
    extends SwaggerHttpService {

  override val host: String = settings.bindAddress + ":" + settings.port
  override val info: Info = Info(
    "The Web Interface to the Plix DEX API",
    Version.VersionString,
    "Plix DEX",
    "License: MIT License",
    None,
    Some(License("MIT License", "https://github.com/plixplatform/dex/blob/master/LICENSE"))
  )

  //Let swagger-ui determine the host and port
  override val swaggerConfig: Swagger = new Swagger()
    .basePath(SwaggerHttpService.prependSlashIfNecessary(basePath))
    .info(info)
    .scheme(Scheme.HTTP)
    .scheme(Scheme.HTTPS)
}
