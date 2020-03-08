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
import gwen.dsl.Tag
import scala.io.Source
import gwen.dsl.GherkinParser
import scala.util.Success
import scala.util.Failure

class FeatureSetTest extends FlatSpec with Matchers with GherkinParser with SpecNormaliser {
  
  "Data driven feature with csv file" should "normalise without error" in {
    val featureFile = new File(getClass.getResource("/gwen/datadriven/AboutMe.feature").getFile)
    val dataFile = new File(getClass.getResource("/gwen/datadriven/AboutMe.csv").getFile)
    val featureSet = new FeatureSet(FeatureUnit(featureFile, Nil, None), dataFile)
    
    featureSet.hasNext should be (true)
    val unit1 = featureSet.next()
    val feature1 = parseFeatureSpec(Source.fromFile(unit1.featureFile).mkString) match {
      case Success(spec) => normalise(spec, Some(unit1.featureFile), unit1.dataRecord)
      case Failure(e) => sys.error(e.toString)
    }
    feature1.feature.name should be ("About me, [1] my age=18..")
    feature1.scenarios.length should be (1)
    feature1.scenarios(0).background.get.name should be ("Input data")
    feature1.scenarios(0).background.get.description should be (List(s"""@Data(file="${dataFile.getPath}", record=1)"""))
    feature1.scenarios(0).background.get.steps(0).toString should be ("""Given my age is "18"""")
    feature1.scenarios(0).background.get.steps(1).toString should be ("""And my gender is "male"""")
    feature1.scenarios(0).background.get.steps(2).toString should be ("""And my title is "Mr"""")
    feature1.scenarios(0).name should be ("What am I?")
    feature1.scenarios(0).steps(0).toString should be ("Given I am ${my age} year(s) old")
    feature1.scenarios(0).steps(1).toString should be ("When I am a ${my gender}")
    feature1.scenarios(0).steps(2).toString should be ("Then I am a ${my age} year old ${my title}")
    
    featureSet.hasNext should be (true)
    val unit2 = featureSet.next()
    val feature2 = parseFeatureSpec(Source.fromFile(unit2.featureFile).mkString) match {
      case Success(spec) => normalise(spec, Some(unit2.featureFile), unit2.dataRecord)
      case Failure(e) => sys.error(e.toString)
    }
    feature2.feature.name should be ("About me, [2] my age=18..")
    feature2.scenarios.length should be (1)
    feature2.scenarios(0).background.get.name should be ("Input data")
    feature2.scenarios(0).background.get.description should be (List(s"""@Data(file="${dataFile.getPath}", record=2)"""))
    feature2.scenarios(0).background.get.steps(0).toString should be ("""Given my age is "18"""")
    feature2.scenarios(0).background.get.steps(1).toString should be ("""And my gender is "female"""")
    feature2.scenarios(0).background.get.steps(2).toString should be ("""And my title is "Miss"""")
    feature2.scenarios(0).name should be ("What am I?")
    feature2.scenarios(0).steps(0).toString should be ("Given I am ${my age} year(s) old")
    feature2.scenarios(0).steps(1).toString should be ("When I am a ${my gender}")
    feature2.scenarios(0).steps(2).toString should be ("Then I am a ${my age} year old ${my title}")
    
    featureSet.hasNext should be (true)
    val unit3 = featureSet.next()
    val feature3 = parseFeatureSpec(Source.fromFile(unit3.featureFile).mkString) match {
      case Success(spec) => normalise(spec, Some(unit3.featureFile), unit3.dataRecord)
      case Failure(e) => sys.error(e.toString)
    }
    feature3.feature.name should be ("About me, [3] my age=22..")
    feature3.scenarios.length should be (1)
    feature3.scenarios(0).background.get.name should be ("Input data")
    feature3.scenarios(0).background.get.description should be (List(s"""@Data(file="${dataFile.getPath}", record=3)"""))
    feature3.scenarios(0).background.get.steps(0).toString should be ("""Given my age is "22"""")
    feature3.scenarios(0).background.get.steps(1).toString should be ("""And my gender is "female"""")
    feature3.scenarios(0).background.get.steps(2).toString should be ("""And my title is "Ms"""")
    feature3.scenarios(0).name should be ("What am I?")
    feature3.scenarios(0).steps(0).toString should be ("Given I am ${my age} year(s) old")
    feature3.scenarios(0).steps(1).toString should be ("When I am a ${my gender}")
    feature3.scenarios(0).steps(2).toString should be ("Then I am a ${my age} year old ${my title}")
    
    featureSet.hasNext should be (false)
    
  }
  
}