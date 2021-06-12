/*
 * Copyright 2020-2021 Branko Juric, Brady Wood
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

 package gwen.core.state

 import gwen.core._
 
import org.scalatest.matchers.should.Matchers

 class SensitiveDataTest extends BaseTest with Matchers {

  "isMaskedName" should "return true for masked proeprty names" in {
    SensitiveData.isMaskedName("""my:masked""") should be (true)
    SensitiveData.isMaskedName("""my.prop:masked""") should be (true)
    SensitiveData.isMaskedName("""my.prop.other:masked""") should be (true)
  }

  "isMaskedName" should "return false for standard proeprty names" in {
    SensitiveData.isMaskedName("""my""") should be (false)
    SensitiveData.isMaskedName("""my.prop""") should be (false)
    SensitiveData.isMaskedName("""my.prop.other""") should be (false)
  }

  "parse" should "return masked value for masked property" in {
    val MaskChar = GwenSettings.`gwen.mask.char`
    val ZeroChar = '‎' // zero width space char
    val parsed = SensitiveData.parse("my.prop:masked", "howdydoo") 
    parsed should not be (None)
    parsed foreach { case (name, value) => 
      name should be ("my.prop")
      val pattern = s"($MaskChar{5})${ZeroChar}+"
      value.matches(pattern) should be (true)
      value.trim.matches(pattern) should be (true)
      value.matches("howdydoo") should be (false)
    }
  }

  "parse" should "return plain original value for non-masked property" in {
    val parsed = SensitiveData.parse("my.raw.prop", "howdydoo") 
    parsed should not be (None)
    parsed foreach { case (name, value) => 
      name should be ("my.raw.prop")
      value should be ("howdydoo")
    }
  }

  "parsing with same name and value" should "not create a duplicate" in {
    val parsed1 = SensitiveData.parse("my.prop.same:masked", "howdydoo") 
    parsed1 should not be (None)
    val parsed2 = SensitiveData.parse("my.prop.same:masked", "howdydoo") 
    parsed2 should be (parsed1)
  }

  "parsing with same name but different value" should "create separate entries" in {
    val parsed1 = SensitiveData.parse("my.prop.changing:masked", "howdy") 
    parsed1 should not be (None)
    val parsed2 = SensitiveData.parse("my.prop.changing:masked", "howdydoo") 
    parsed2 should not be (parsed1)
  }

  "withValue" should "pass in unmasked value" in {
    withSetting("my.prop:masked", "howdydoo") {
      val unmasked = SensitiveData.withValue(sys.props("my.prop")) { identity }
      unmasked should be ("howdydoo")
    }
  }

  "withValue" should "return unknown masked value as is" in {
    val MaskChar = GwenSettings.`gwen.mask.char`
    val ZeroChar = '‎' // zero width space char
    val unknown = s"${MaskChar.toString * 5}${ZeroChar.toString * 42}"
    val unmasked = SensitiveData.withValue(unknown) { identity }
    unmasked should be (unknown)
  }

  "withValue" should "unmask masked value" in {
    withSetting("my.prop:masked", "howdydoo") {
      val unmasked = SensitiveData.withValue(s"Well ${sys.props("my.prop")} partner!") { identity }
      unmasked should be ("Well howdydoo partner!")
    }
  }

  "withValue" should "unmask multiple masked values" in {
    withSetting("my.prop.1:masked", "howdydoo") {
      withSetting("my.prop.2:masked", "partner") {
        val unmasked = SensitiveData.withValue(s"Well ${sys.props("my.prop.1")} ${sys.props("my.prop.2")}!") { identity }
        unmasked should be ("Well howdydoo partner!")
      }
    }
  }

  "withValue" should "unmask multiple masked values in a multiline string" in {
    withSetting("my.prop.1:masked", "howdydoo") {
      withSetting("my.prop.2:masked", "partner") {
        val unmasked = SensitiveData.withValue(
          s"""|Well
              | ${sys.props("my.prop.1")}
              |  ${sys.props("my.prop.2")}!""".stripMargin) { identity }
        unmasked should be (
          """|Well
             | howdydoo
             |  partner!""".stripMargin
        )
      }
    }
  }

}
