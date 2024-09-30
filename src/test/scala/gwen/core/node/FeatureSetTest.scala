/*
 * Copyright 2015-2024 Branko Juric, Brady Wood
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

package gwen.core.node

import gwen.core.BaseTest
import gwen.core.GwenOptions
import gwen.core.data.DataSource
import gwen.core.node.gherkin.GherkinParser
import gwen.core.node.gherkin.SpecNormaliser
import gwen.core.node.gherkin.TagFilter

import org.scalatest.matchers.should.Matchers

import scala.util.Success
import scala.util.Failure

import java.io.File

class FeatureSetTest extends BaseTest with Matchers with GherkinParser with SpecNormaliser {

  private val options = GwenOptions(dryRun = false, parallel = false)
  
  "Data driven feature with csv data file" should "normalise without error" in {
    verify3("/gwen/datadriven/AboutMe.csv")
  }

  "Data driven feature with top array json data file" should "normalise without error" in {
    verify3("/gwen/datadriven/AboutMeTopArray.json")
  }

  "Data driven feature with top object json data file" should "normalise without error" in {
    verify1("/gwen/datadriven/AboutMeTopObject.json")
  }

  private def verify3(dataFilePath: String): Unit = {

    val featureFile = new File(getClass.getResource("/gwen/datadriven/AboutMe.feature").getFile)
    val dataFile = new File(getClass.getResource(dataFilePath).getFile)
    val featureSet = new FeatureSet(FeatureUnit(Root, featureFile, Nil, None, new TagFilter(Nil)), DataSource(dataFile))

    featureSet.hasNext should be (true)
    val unit1 = featureSet.next()
    val feature1 = parseSpec(unit1.featureFile) match {
      case Success(spec) => normaliseSpec(spec, unit1.dataRecord, options)
      case Failure(e) => sys.error(e.toString)
    }
    feature1.feature.name should be ("About me")
    feature1.feature.displayName should be ("About me [1 of 3]")
    feature1.scenarios.length should be (1)
    feature1.scenarios(0).background.get.name should be ("Input data record")
    feature1.scenarios(0).background.get.displayName should be ("Input data record [1 of 3]")
    feature1.scenarios(0).background.get.description should be (List(s"Input data file: ${dataFile.getPath}"))
    feature1.scenarios(0).background.get.steps(0).toString should be ("""Given my age is "18"""")
    feature1.scenarios(0).background.get.steps(1).toString should be ("""And my gender is "male"""")
    feature1.scenarios(0).background.get.steps(2).toString should be ("""And my title is "Mr"""")
    feature1.scenarios(0).name should be ("What am I?")
    feature1.scenarios(0).steps(0).toString should be ("Given I am ${my age} year(s) old")
    feature1.scenarios(0).steps(1).toString should be ("When I am a ${my gender}")
    feature1.scenarios(0).steps(2).toString should be ("Then I am a ${my age} year old ${my title}")
    
    featureSet.hasNext should be (true)
    val unit2 = featureSet.next()
    val feature2 = parseSpec(unit2.featureFile) match {
      case Success(spec) => normaliseSpec(spec, unit2.dataRecord, options)
      case Failure(e) => sys.error(e.toString)
    }
    feature2.feature.name should be ("About me")
    feature2.feature.displayName should be ("About me [2 of 3]")
    feature2.scenarios.length should be (1)
    feature2.scenarios(0).background.get.name should be ("Input data record")
    feature2.scenarios(0).background.get.displayName should be ("Input data record [2 of 3]")
    feature2.scenarios(0).background.get.description should be (List(s"Input data file: ${dataFile.getPath}"))
    feature2.scenarios(0).background.get.steps(0).toString should be ("""Given my age is "18"""")
    feature2.scenarios(0).background.get.steps(1).toString should be ("""And my gender is "female"""")
    feature2.scenarios(0).background.get.steps(2).toString should be ("""And my title is "Miss"""")
    feature2.scenarios(0).name should be ("What am I?")
    feature2.scenarios(0).steps(0).toString should be ("Given I am ${my age} year(s) old")
    feature2.scenarios(0).steps(1).toString should be ("When I am a ${my gender}")
    feature2.scenarios(0).steps(2).toString should be ("Then I am a ${my age} year old ${my title}")
    
    featureSet.hasNext should be (true)
    val unit3 = featureSet.next()
    val feature3 = parseSpec(unit3.featureFile) match {
      case Success(spec) => normaliseSpec(spec, unit3.dataRecord, options)
      case Failure(e) => sys.error(e.toString)
    }
    feature3.feature.name should be ("About me")
    feature3.feature.displayName should be ("About me [3 of 3]")
    feature3.scenarios.length should be (1)
    feature3.scenarios(0).background.get.name should be ("Input data record")
    feature3.scenarios(0).background.get.displayName should be ("Input data record [3 of 3]")
    feature3.scenarios(0).background.get.description should be (List(s"Input data file: ${dataFile.getPath}"))
    feature3.scenarios(0).background.get.steps(0).toString should be ("""Given my age is "22"""")
    feature3.scenarios(0).background.get.steps(1).toString should be ("""And my gender is "female"""")
    feature3.scenarios(0).background.get.steps(2).toString should be ("""And my title is "Ms"""")
    feature3.scenarios(0).name should be ("What am I?")
    feature3.scenarios(0).steps(0).toString should be ("Given I am ${my age} year(s) old")
    feature3.scenarios(0).steps(1).toString should be ("When I am a ${my gender}")
    feature3.scenarios(0).steps(2).toString should be ("Then I am a ${my age} year old ${my title}")
    
    featureSet.hasNext should be (false)
    
  }

  private def verify1(dataFilePath: String): Unit = {

    val featureFile = new File(getClass.getResource("/gwen/datadriven/AboutMe.feature").getFile)
    val dataFile = new File(getClass.getResource(dataFilePath).getFile)
    val featureSet = new FeatureSet(FeatureUnit(Root, featureFile, Nil, None, new TagFilter(Nil)), DataSource(dataFile))

    featureSet.hasNext should be (true)
    val unit1 = featureSet.next()
    val feature1 = parseSpec(unit1.featureFile) match {
      case Success(spec) => normaliseSpec(spec, unit1.dataRecord, options)
      case Failure(e) => sys.error(e.toString)
    }

    feature1.feature.name should be ("About me")
    feature1.feature.displayName should be ("About me [1 of 1]")
    feature1.scenarios.length should be (1)
    feature1.scenarios(0).background.get.name should be ("Input data record")
    feature1.scenarios(0).background.get.displayName should be ("Input data record [1 of 1]")
    feature1.scenarios(0).background.get.description should be (List(s"Input data file: ${dataFile.getPath}"))
    feature1.scenarios(0).background.get.steps(0).toString should be ("""Given my age is "18"""")
    feature1.scenarios(0).background.get.steps(1).toString should be ("""And my gender is "male"""")
    feature1.scenarios(0).background.get.steps(2).toString should be ("""And my title is "Mr"""")
    feature1.scenarios(0).name should be ("What am I?")
    feature1.scenarios(0).steps(0).toString should be ("Given I am ${my age} year(s) old")
    feature1.scenarios(0).steps(1).toString should be ("When I am a ${my gender}")
    feature1.scenarios(0).steps(2).toString should be ("Then I am a ${my age} year old ${my title}")
    
    featureSet.hasNext should be (false)
    
  }
  
}