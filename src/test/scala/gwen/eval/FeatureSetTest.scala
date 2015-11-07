/*
 * Copyright 2015 Branko Juric, Brady Wood
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

package gwen.eval

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import gwen.dsl.FeatureSpec
import gwen.dsl.Feature
import gwen.dsl.Scenario
import gwen.dsl.Tag
import gwen.dsl.Step
import gwen.dsl.StepKeyword
import gwen.dsl.SpecParser
import scala.io.Source
import gwen.dsl.SpecNormaliser

class FeatureSetTest extends FlatSpec with Matchers with SpecParser with SpecNormaliser {
  
  "Data driven feature with csv file" should "normalise without error" in {
    val featureFile = new File(getClass().getResource("/gwen/datadriven/AboutMe.feature").getFile())
    val dataFile = new File(getClass().getResource("/gwen/datadriven/AboutMe.csv").getFile())
    val featureSet = new FeatureSet(new FeatureUnit(featureFile, Nil, None), dataFile)
    
    featureSet.hasNext should be (true)
    val unit1 = featureSet.next
    val feature1 = parseAll(spec, Source.fromFile(unit1.featureFile).mkString) match {
      case success @ Success(spec, _) => normalise(spec, Some(unit1.featureFile), unit1.dataRecord)
      case failure: NoSuccess => sys.error(failure.toString)
    }
    feature1.feature.name should be ("About me, [1] my age=18..")
    feature1.scenarios.length should be (2)
    feature1.scenarios(0).tags should contain (Tag(s"""Data(file="${dataFile.getPath}", record=1)"""))
    feature1.scenarios(0).name.endsWith("Bind data attributes")
    feature1.scenarios(0).steps(0).toString should be ("""Given my age is "18"""")
    feature1.scenarios(0).steps(1).toString should be ("""And my gender is "male"""")
    feature1.scenarios(0).steps(2).toString should be ("""And my title is "Mr"""")
    feature1.scenarios(1).name should be ("What am I?")
    feature1.scenarios(1).steps(0).toString should be ("Given I am ${my age} year(s) old")
    feature1.scenarios(1).steps(1).toString should be ("When I am a ${my gender}")
    feature1.scenarios(1).steps(2).toString should be ("Then I am a ${my age} year old ${my title}")
    
    featureSet.hasNext should be (true)
    val unit2 = featureSet.next
    val feature2 = parseAll(spec, Source.fromFile(unit2.featureFile).mkString) match {
      case success @ Success(spec, _) => normalise(spec, Some(unit2.featureFile), unit2.dataRecord)
      case failure: NoSuccess => sys.error(failure.toString)
    }
    feature2.feature.name should be ("About me, [2] my age=18..")
    feature2.scenarios.length should be (2)
    feature2.scenarios(0).tags should contain (Tag(s"""Data(file="${dataFile.getPath}", record=2)"""))
    feature2.scenarios(0).name should be ("Bind data attributes")
    feature2.scenarios(0).steps(0).toString should be ("""Given my age is "18"""")
    feature2.scenarios(0).steps(1).toString should be ("""And my gender is "female"""")
    feature2.scenarios(0).steps(2).toString should be ("""And my title is "Miss"""")
    feature2.scenarios(1).name should be ("What am I?")
    feature2.scenarios(1).steps(0).toString should be ("Given I am ${my age} year(s) old")
    feature2.scenarios(1).steps(1).toString should be ("When I am a ${my gender}")
    feature2.scenarios(1).steps(2).toString should be ("Then I am a ${my age} year old ${my title}")
    
    featureSet.hasNext should be (true)
    val unit3 = featureSet.next
    val feature3 = parseAll(spec, Source.fromFile(unit3.featureFile).mkString) match {
      case success @ Success(spec, _) => normalise(spec, Some(unit3.featureFile), unit3.dataRecord)
      case failure: NoSuccess => sys.error(failure.toString)
    }
    feature3.feature.name should be ("About me, [3] my age=22..")
    feature3.scenarios.length should be (2)
    feature3.scenarios(0).tags should contain (Tag(s"""Data(file="${dataFile.getPath}", record=3)"""))
    feature3.scenarios(0).name should be ("Bind data attributes")
    feature3.scenarios(0).steps(0).toString should be ("""Given my age is "22"""")
    feature3.scenarios(0).steps(1).toString should be ("""And my gender is "female"""")
    feature3.scenarios(0).steps(2).toString should be ("""And my title is "Ms"""")
    feature3.scenarios(1).name should be ("What am I?")
    feature3.scenarios(1).steps(0).toString should be ("Given I am ${my age} year(s) old")
    feature3.scenarios(1).steps(1).toString should be ("When I am a ${my gender}")
    feature3.scenarios(1).steps(2).toString should be ("Then I am a ${my age} year old ${my title}")
    
    featureSet.hasNext should be (false)
    
  }
  
}