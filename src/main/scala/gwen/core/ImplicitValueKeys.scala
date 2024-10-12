/*
 * Copyright 2024 Branko Juric, Brady Wood
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gwen.core

trait ImplicitValueKeys {

    val `gwen.feature.`                    = "gwen.feature."
    val `gwen.feature.language`            = "gwen.feature.language"
    val `gwen.feature.name`                = "gwen.feature.name"
    val `gwen.feature.displayName`         = "gwen.feature.displayName"
    val `gwen.feature.file.name`           = "gwen.feature.file.name"
    val `gwen.feature.file.simpleName`     = "gwen.feature.file.simpleName"
    val `gwen.feature.file.path`           = "gwen.feature.file.path"
    val `gwen.feature.file.absolutePath`   = "gwen.feature.file.absolutePath"
    val `gwen.feature.eval.status.keyword` = "gwen.feature.eval.status.keyword"
    val `gwen.feature.eval.status.message` = "gwen.feature.eval.status.message"
    val `gwen.feature.eval.start.msecs`    = "gwen.feature.eval.start.msecs"
    val `gwen.feature.eval.started`        = "gwen.feature.eval.started"
    val `gwen.feature.eval.finished`       = "gwen.feature.eval.finished"

    val `gwen.rule.`                    = "gwen.rule."
    val `gwen.rule.name`                = "gwen.rule.name"
    val `gwen.rule.eval.start.msecs`    = "gwen.rule.eval.start.msecs"
    val `gwen.rule.eval.started`        = "gwen.rule.eval.started"
    val `gwen.rule.eval.finished`       = "gwen.rule.eval.finished"
    val `gwen.rule.eval.status.keyword` = "gwen.rule.eval.status.keyword"

    val `gwen.scenario.`                    = "gwen.scenario."
    val `gwen.scenario.name`                = "gwen.scenario.name"
    val `gwen.scenario.displayName`         = "gwen.scenario.displayName"
    val `gwen.scenario.eval.start.msecs`    = "gwen.scenario.eval.start.msecs"
    val `gwen.scenario.eval.started`        = "gwen.scenario.eval.started"
    val `gwen.scenario.eval.finished`       = "gwen.scenario.eval.finished"
    val `gwen.scenario.eval.status.keyword` = "gwen.scenario.eval.status.keyword"

    val `gwen.examples.`                    = "gwen.examples."
    val `gwen.examples.name`                = "gwen.examples.name"
    val `gwen.examples.eval.start.msecs`    = "gwen.examples.eval.start.msecs"
    val `gwen.examples.eval.started`        = "gwen.examples.eval.started"
    val `gwen.examples.eval.finished`       = "gwen.examples.eval.finished"
    val `gwen.examples.eval.status.keyword` = "gwen.examples.eval.status.keyword"

    val `gwen.stepDef.`                    = "gwen.stepDef."
    val `gwen.stepDef.name`                = "gwen.stepDef.name"
    val `gwen.stepDef.displayName`         = "gwen.stepDef.displayName"
    val `gwen.stepDef.eval.start.msecs`    = "gwen.stepDef.eval.start.msecs"
    val `gwen.stepDef.eval.started`        = "gwen.stepDef.eval.started"
    val `gwen.stepDef.eval.finished`       = "gwen.stepDef.eval.finished"
    val `gwen.stepDef.eval.status.keyword` = "gwen.stepDef.eval.status.keyword"

    val `gwen.accumulated.errors`           = "gwen.accumulated.errors"
    val `gwen.accumulated.errors:JSONArray` = "gwen.accumulated.errors:JSONArray"

    val `gwen.iteration.number` = "gwen.iteration.number"
    val `gwen.iteration.index`  = "gwen.iteration.index"

    val `data.record.`       = "data.record."
    
    val `gwen.data.record.number` = "gwen.data.record.number"
    val `gwen.data.record.index`  = "gwen.data.record.index"

    val `gwen.table.record.number` = "gwen.table.record.number"
    val `gwen.table.record.index`  = "gwen.table.record.index"
    
    val `gwen.report.results.errors` = "gwen.report.results.errors"

}