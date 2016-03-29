/*
 * Copyright 2012-2013 Stephane Godbillon (@sgodbillon)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package reactivemongo.play.json.commands

import play.api.libs.json.{ JsValue, Json }, Json.toJson

import reactivemongo.api.commands.AggregationFramework
import reactivemongo.play.json.JSONSerializationPack

object JSONAggregationFramework
    extends AggregationFramework[JSONSerializationPack.type] {

  import Json.JsValueWrapper

  val pack: JSONSerializationPack.type = JSONSerializationPack

  protected def makeDocument(elements: Seq[(String, JsValueWrapper)]) =
    Json.obj(elements: _*)

  protected def elementProducer(name: String, value: JsValue) =
    name -> value

  protected def booleanValue(b: Boolean): JsValue = toJson(b)
  protected def intValue(i: Int): JsValue = toJson(i)
  protected def longValue(l: Long): JsValue = toJson(l)
  protected def doubleValue(d: Double): JsValue = toJson(d)
  protected def stringValue(s: String): JsValue = toJson(s)
}

object JSONAggregationImplicits {
  import play.api.libs.json.{ JsArray, JsObject, JsValue, OWrites }
  import reactivemongo.api.commands.ResolvedCollectionCommand
  import reactivemongo.core.protocol.MongoWireVersion
  import JSONAggregationFramework.{ Aggregate, AggregationResult }
  import reactivemongo.play.json.BSONFormats

  implicit object AggregateWriter
      extends OWrites[ResolvedCollectionCommand[Aggregate]] {
    def writes(agg: ResolvedCollectionCommand[Aggregate]): JsObject = {
      val fields = Seq[(String, JsValue)](
        "aggregate" -> toJson(agg.collection),
        "pipeline" -> JsArray(agg.command.pipeline.map(_.makePipe)),
        "explain" -> toJson(agg.command.explain),
        "allowDiskUse" -> toJson(agg.command.allowDiskUse)
      )

      def optFields: List[Option[(String, JsValue)]] = List(
        agg.command.cursor.map(c => "cursor" -> Json.obj(
          "batchSize" -> c.batchSize
        ))
      )

      def options = {
        if (agg.command.wireVersion < MongoWireVersion.V32) optFields.flatten
        else (optFields ++ List(
          Some(agg.command.bypassDocumentValidation).map(by =>
            "bypassDocumentValidation" -> toJson(by)),
          agg.command.readConcern.map(rc => "readConcern" -> toJson(rc.level))
        )).flatten
      }

      JsObject(fields ++ options)
    }
  }

  implicit object AggregationResultReader
      extends DealingWithGenericCommandErrorsReader[AggregationResult] {
    def readResult(doc: JsObject): AggregationResult =
      AggregationResult((doc \ "result").as[List[JsObject]])

  }
}
