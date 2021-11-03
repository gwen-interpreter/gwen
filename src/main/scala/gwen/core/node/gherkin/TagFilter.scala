/*
 * Copyright 2014-2021 Branko Juric, Brady Wood
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

package gwen.core.node.gherkin

/**
  * Checks that a feature satisfies all user provided include/exclude tags.
  * Features that do not satisfy any of the provided tags are filtered out 
  * and returned as None types. Features having scenarios that satisfy all 
  * tags are pruned of all scenarios that do not and are wrapped and 
  * returned as Some types.
  * 
  * @param tagFilters user provided tag filters (includes:(tag, true) and excludes:(tag, false))
  */

object TagFilter {
  private val DefaultFilters = List((Tag(ReservedTags.Ignore), false))
}

class TagFilter(tagFilters: List[(Tag, Boolean)]) {
  
  /**
    * Filters a feature using the given include/exclude tag filters.
    * 
    * @param spec the parsed feature spec to check
    * @return None if the given feature does not have any scenarios that satisfy all tags; 
    *         Some otherwise (with only the scenarios that do)
    */
  def filter(spec: Spec): Option[Spec] = { 
    def scenarios = filterScenarios(spec, spec.scenarios)
    def rules = spec.rules map { rule =>
      rule.copy(withScenarios = filterScenarios(spec, rule.scenarios))
    }
    if (scenarios.isEmpty && rules.forall(_.scenarios.isEmpty)) {
      None
    } else {
      Some(spec.copy(
        withScenarios = scenarios, 
        withRules = rules))
    }
  }

  private def filterScenarios(spec: Spec, scenarios: List[Scenario]): List[Scenario] = {
    val filters = tagFilters ++ TagFilter.DefaultFilters
    scenarios flatMap { scenario =>
      val effectiveTags = spec.feature.tags ++ scenario.tags
      val (includes, excludes) = filters.partition(_._2) match { case(x, y) => (x.map(_._1.name ), y.map(_._1.name ))}
      if (isSatisfied(effectiveTags, includes, excludes) || isSatisfied(effectiveTags ++ scenario.examples.flatMap(_.tags), includes, excludes)) {
        val filteredExamples = scenario.examples.filter { examples => isSatisfied(effectiveTags ++ examples.tags, includes, excludes) }
        if (filteredExamples != scenario.examples) {
          if (filteredExamples.isEmpty) {
            None
          } else {
            Some(scenario.copy(withExamples = filteredExamples))
          }
        } else {
          Some(scenario)
        }
      }
      else None
    }
  }

  private def isSatisfied(tags: List[Tag], includes: List[String], excludes: List[String]): Boolean = {
    val includeSatisfied = includes.isEmpty || tags.map(_.name).exists(name => includes.contains(name))
    val excludeSatisfied = excludes.isEmpty || tags.map(_.name).forall(name => !excludes.contains(name))
    includeSatisfied && excludeSatisfied
  }
  
}