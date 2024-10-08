/*
 * Copyright 2015-2021 Branko Juric, Brady Wood
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

import gwen.core.BaseTest
import gwen.core.Formatting.formatDuration
import gwen.core.Formatting.parseDuration

import scala.concurrent.duration.Duration

import org.scalatest.matchers.should.Matchers

class DurationFormatterTest extends BaseTest with Matchers {

  "Zero duration" should "format to ~0ms" in {
    formatDuration(Duration.fromNanos(0)) should be ("~0ms")
  }
  
  "1 millisecond duration" should "format to 1ms" in {
    formatDuration(Duration.fromNanos(1000000)) should be ("1ms")
  }
  
  "1 second duration" should "format to " in {
    formatDuration(Duration.fromNanos(1000000000)) should be ("1s 000ms")
  }
   
  "1 second 1 millisecond duration" should "format to " in {
    formatDuration(Duration.fromNanos(1001000000)) should be ("1s 001ms")
  }
  
  "1 minute duration" should "format to " in {
    formatDuration(Duration.fromNanos(60000000000L)) should be ("1m 00s 000ms")
  }
  
  "1 minute 1 second 1 millisecond duration" should "format to " in {
    formatDuration(Duration.fromNanos(61001000000L)) should be ("1m 01s 001ms")
  }

  "1 hour duration" should "format to " in { 
    formatDuration(Duration.fromNanos(3600000000000L)) should be ("1h 00m 00s 000ms")
  }
    
  "1 hour 1 minute duration" should "format to " in {
    formatDuration(Duration.fromNanos(3660000000000L)) should be ("1h 01m 00s 000ms")
  }
  
  "1 hour 1 minute 1 second duration" should "format to " in {
    formatDuration(Duration.fromNanos(3661000000000L)) should be ("1h 01m 01s 000ms")
  }
  
  "1 hour 1 minute 1 second 1 millisecond duration" should "format to " in {
    formatDuration(Duration.fromNanos(3661001000000L)) should be ("1h 01m 01s 001ms")
  }
  
  "1 hour 1 minute 1 second 1 millisecond 1 microsecond duration" should "round down to nearest millisecond and format to " in {
    formatDuration(Duration.fromNanos(3661001000001L)) should be ("1h 01m 01s 001ms")
  }
  
  "1 hour 1 minute 1 second 1 millisecond 499999 microseconds duration" should "round down to nearest millisecond and format to " in {
    formatDuration(Duration.fromNanos(3661001499999L)) should be ("1h 01m 01s 001ms")
  }
  
  "1 hour 1 minute 1 second 1 millisecond 500001 microseconds duration" should "round up to nearest millisecond and format to " in {
    formatDuration(Duration.fromNanos(3661001500000L)) should be ("1h 01m 01s 002ms")
  }
  
  "1 hour 1 minute 1 second 1 millisecond 999999 microseconds duration" should "round up to nearest millisecond and format to " in {
    formatDuration(Duration.fromNanos(3661001999999L)) should be ("1h 01m 01s 002ms")
  }

  "1h1m1s1ms" should "parse" in {
    parseDuration("1h1m1s1ms").map(_.toMillis) should be (Some(3661001L))
  }

  "2h1m1ms" should "parse" in {
    parseDuration("2h1m1ms").map(_.toMillis) should be (Some(7260001L))
  }

  "1h5s" should "parse" in {
    parseDuration("1h5s").map(_.toMillis) should be (Some(3605000L))
  }

  "2h01ms" should "parse" in {
    parseDuration("2h01ms").map(_.toMillis) should be (Some(7200001L))
  }

  "2m" should "parse" in {
    parseDuration("2m").map(_.toMillis) should be (Some(120000L))
  }

  "5m10s" should "parse" in {
    parseDuration("5m10s").map(_.toMillis) should be (Some(310000L))
  }

  "500ms" should "parse" in {
    parseDuration("500ms").map(_.toMillis) should be (Some(500L))
  }

  "1 minute" should "not parse" in {
    parseDuration("1 minute") should be (None)
  }

  "2secs" should "not parse" in {
    parseDuration("2secs") should be (None)
  }

}