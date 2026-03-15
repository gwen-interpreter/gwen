/*
 * Copyright 2026 Branko Juric, Brady Wood
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

package gwen.core.eval.support

import gwen.core._

import org.scalatest.matchers.should.Matchers

import scala.util.chaining._

class TextFormatSupportTest extends BaseTest with Matchers with TextFormatSupport {

  "ISO date" should "convert to DMY date" in {
    formatDateTime("2026-03-15", "yyyy-MM-dd", "dd/MM/yyyy") should be ("15/03/2026")
  }

  "ISO date" should "convert to date with ordinal day suffix" in {
    formatDateTime("2026-03-01", "yyyy-MM-dd", "d(st|nd|rd|th) MMMM yyyy") should be ("1st March 2026")
    formatDateTime("2026-03-02", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("2nd Mar 2026")
    formatDateTime("2026-03-03", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("3rd Mar 2026")
    formatDateTime("2026-03-04", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("4th Mar 2026")
    formatDateTime("2026-03-10", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("10th Mar 2026")
    formatDateTime("2026-03-11", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("11th Mar 2026")
    formatDateTime("2026-03-12", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("12th Mar 2026")
    formatDateTime("2026-03-13", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("13th Mar 2026")
    formatDateTime("2026-03-14", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("14th Mar 2026")
    formatDateTime("2026-03-20", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("20th Mar 2026")
    formatDateTime("2026-03-21", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("21st Mar 2026")
    formatDateTime("2026-03-22", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("22nd Mar 2026")
    formatDateTime("2026-03-23", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("23rd Mar 2026")
    formatDateTime("2026-03-24", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("24th Mar 2026")
    formatDateTime("2026-03-30", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("30th Mar 2026")
    formatDateTime("2026-03-31", "yyyy-MM-dd", "d(st|nd|rd|th) MMM yyyy") should be ("31st Mar 2026") 
  }

  "Date with ordinal day suffix" should "convert to ISO" in {
    formatDateTime("1st Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-01")
    formatDateTime("2nd Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-02")
    formatDateTime("3rd Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-03")
    formatDateTime("4th Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-04")
    formatDateTime("10th Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-10")
    formatDateTime("11th Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-11")
    formatDateTime("12th Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-12")
    formatDateTime("13th Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-13")
    formatDateTime("14th Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-14")
    formatDateTime("20th Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-20")
    formatDateTime("21st Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-21")
    formatDateTime("22nd Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-22")
    formatDateTime("23rd Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-23")
    formatDateTime("24th Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-24")
    formatDateTime("30th Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-30")
    formatDateTime("31st Mar 2026", "d(st|nd|rd|th) MMM yyyy", "yyyy-MM-dd") should be ("2026-03-31") 
  }

  "Number" should "convert to dollars" in {
    formatNumber("1234", "#", "$#,##0.00") should be ("$1,234.00")
  }

  "Number" should "convert to number with orinal suffix" in {
    formatNumber("00", "#", "#(st|nd|rd|th)") should be ("0th")
    formatNumber("01", "#", "#(st|nd|rd|th)") should be ("1st")
    formatNumber("02", "#", "#(st|nd|rd|th)") should be ("2nd")
    formatNumber("03", "#", "#(st|nd|rd|th)") should be ("3rd")
    formatNumber("04", "#", "#(st|nd|rd|th)") should be ("4th")
    formatNumber("10", "#", "#(st|nd|rd|th)") should be ("10th")
    formatNumber("11", "#", "#(st|nd|rd|th)") should be ("11th")
    formatNumber("12", "#", "#(st|nd|rd|th)") should be ("12th")
    formatNumber("13", "#", "#(st|nd|rd|th)") should be ("13th")
    formatNumber("14", "#", "#(st|nd|rd|th)") should be ("14th")
    formatNumber("20", "#", "#(st|nd|rd|th)") should be ("20th")
    formatNumber("21", "#", "#(st|nd|rd|th)") should be ("21st")
    formatNumber("22", "#", "#(st|nd|rd|th)") should be ("22nd")
    formatNumber("23", "#", "#(st|nd|rd|th)") should be ("23rd")
    formatNumber("24", "#", "#(st|nd|rd|th)") should be ("24th")
    formatNumber("30", "#", "#(st|nd|rd|th)") should be ("30th")
    formatNumber("31", "#", "#(st|nd|rd|th)") should be ("31st")
    formatNumber("100", "#", "#(st|nd|rd|th)") should be ("100th")
    formatNumber("201", "#", "#(st|nd|rd|th)") should be ("201st")
    formatNumber("302", "#", "#(st|nd|rd|th)") should be ("302nd")
    formatNumber("403", "#", "#(st|nd|rd|th)") should be ("403rd")
    formatNumber("504", "#", "#(st|nd|rd|th)") should be ("504th")
    formatNumber("610", "#", "#(st|nd|rd|th)") should be ("610th")
    formatNumber("711", "#", "#(st|nd|rd|th)") should be ("711th")
    formatNumber("812", "#", "#(st|nd|rd|th)") should be ("812th")
    formatNumber("913", "#", "#(st|nd|rd|th)") should be ("913th")
    formatNumber("1014", "#", "#(st|nd|rd|th)") should be ("1014th")
    formatNumber("1120", "#", "#(st|nd|rd|th)") should be ("1120th")
    formatNumber("1221", "#", "#(st|nd|rd|th)") should be ("1221st")
    formatNumber("1322", "#", "#(st|nd|rd|th)") should be ("1322nd")
    formatNumber("1423", "#", "#(st|nd|rd|th)") should be ("1423rd")
    formatNumber("1524", "#", "#(st|nd|rd|th)") should be ("1524th")
    formatNumber("1630", "#", "#(st|nd|rd|th)") should be ("1630th")
    formatNumber("1731", "#", "#(st|nd|rd|th)") should be ("1731st")
  }

  "Number with ordinal suffix" should "convert to number" in {
    formatNumber("0th", "#(st|nd|rd|th)", "00") should be ("00")
    formatNumber("1st", "#(st|nd|rd|th)", "00") should be ("01")
    formatNumber("2nd", "#(st|nd|rd|th)", "00") should be ("02")
    formatNumber("3rd", "#(st|nd|rd|th)", "00") should be ("03")
    formatNumber("4th", "#(st|nd|rd|th)", "00") should be ("04")
    formatNumber("10th", "#(st|nd|rd|th)", "#") should be ("10")
    formatNumber("11th", "#(st|nd|rd|th)", "#") should be ("11")
    formatNumber("12th", "#(st|nd|rd|th)", "#") should be ("12")
    formatNumber("13th", "#(st|nd|rd|th)", "#") should be ("13")
    formatNumber("14th", "#(st|nd|rd|th)", "#") should be ("14")
    formatNumber("20th", "#(st|nd|rd|th)", "#") should be ("20")
    formatNumber("21st", "#(st|nd|rd|th)", "#") should be ("21")
    formatNumber("22nd", "#(st|nd|rd|th)", "#") should be ("22")
    formatNumber("23rd", "#(st|nd|rd|th)", "#") should be ("23")
    formatNumber("24th", "#(st|nd|rd|th)", "#") should be ("24")
    formatNumber("30th", "#(st|nd|rd|th)", "#") should be ("30")
    formatNumber("31st", "#(st|nd|rd|th)", "#") should be ("31")
    formatNumber("100th", "#(st|nd|rd|th)", "#") should be ("100")
    formatNumber("201st", "#(st|nd|rd|th)", "#") should be ("201")
    formatNumber("302nd", "#(st|nd|rd|th)", "#") should be ("302")
    formatNumber("403rd", "#(st|nd|rd|th)", "#") should be ("403")
    formatNumber("504th", "#(st|nd|rd|th)", "#") should be ("504")
    formatNumber("610th", "#(st|nd|rd|th)", "#") should be ("610")
    formatNumber("711st", "#(st|nd|rd|th)", "#") should be ("711")
    formatNumber("812nd", "#(st|nd|rd|th)", "#") should be ("812")
    formatNumber("913rd", "#(st|nd|rd|th)", "#") should be ("913")
    formatNumber("1014th", "#(st|nd|rd|th)", "#") should be ("1014")
    formatNumber("1120th", "#(st|nd|rd|th)", "#") should be ("1120")
    formatNumber("1221st", "#(st|nd|rd|th)", "#") should be ("1221")
    formatNumber("1322nd", "#(st|nd|rd|th)", "#") should be ("1322")
    formatNumber("1423rd", "#(st|nd|rd|th)", "#") should be ("1423")
    formatNumber("1524th", "#(st|nd|rd|th)", "#") should be ("1524")
    formatNumber("1630th", "#(st|nd|rd|th)", "#") should be ("1630")
    formatNumber("1731st", "#(st|nd|rd|th)", "#") should be ("1731")
  }

}
