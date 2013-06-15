package com.mosesn.fql

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.twitter.util.Time
import com.twitter.util.TimeConversions.intToTimeableNumber

class ParserSpec extends FunSpec with ShouldMatchers {
  describe("Parser") {
    it("should parse WHAT WILL BE THE") {
      FutureParser.parseAll(FutureParser.select, "WHAT WILL BE THE").get should be ("WHAT WILL BE THE")
    }

    it("should parse weather") {
      val actual = FutureParser.parseAll(FutureParser.selectors, "weather").get
      actual should be (Seq("weather"))
    }

    it("should parse columns weather") {
      val actual = FutureParser.parseAll(FutureParser.columns, "weather").get
      actual should be (Seq("weather"))
    }

    it("should parse WHAT WILL BE THE weather") {
      val actual = FutureParser.handle(FutureParser.parseAll(FutureParser.selectClause, "WHAT WILL BE THE weather"))
      actual should be (WhatWill(Seq("weather")))
    }

    it("should parse WHAT WILL BE THE *") {
      val actual = FutureParser.handle(FutureParser.parseAll(FutureParser.selectClause, "WHAT WILL BE THE *"))
      actual should be (WhatWill(Seq()))
    }

    it("should parse ON") {
      FutureParser.parseAll(FutureParser.on, "ON").get should be ("ON")
    }

    it("should parse time") {
      val ts = Time.now
      (FutureParser.parseAll(FutureParser.timestamp, ts.inSeconds.toString).get - ts).inSeconds should be (0)
    }

    it("should parse ON time") {
      val ts = Time.fromSeconds(Time.now.inSeconds)
      FutureParser.handle(FutureParser.parseAll(FutureParser.whenClause, "ON %s" format ts.inSeconds.toString)) should be (When(ts, None))
    }

    it("should parse UNTIL time") {
      val ts = Time.fromSeconds(Time.now.inSeconds)
      FutureParser.handle(FutureParser.parseAll(FutureParser.untilClause, "UNTIL %s" format ts.inSeconds.toString)) should be (ts)
    }

    it("should parse ON time UNTIL time") {
      val ts = Time.fromSeconds(Time.now.inSeconds)
      FutureParser.handle(FutureParser.parseAll(FutureParser.whenClause, "ON %s UNTIL %s" format ((ts - 5.seconds).inSeconds.toString, ts.inSeconds.toString))) should be (When(ts - 5.seconds, Some(5.seconds)))
    }

    it("should parse WHERE") {
      FutureParser.parseAll(FutureParser.where, "WHERE").get should be ("WHERE")
    }

    it("should parse city=nyc") {
      FutureParser.parseAll(FutureParser.keyValue, "city=nyc").get should be ("city" -> "nyc")
    }

    it("should parse city=nyc as many") {
      FutureParser.parseAll(FutureParser.keyValues, "city=nyc").get should be (Seq("city" -> "nyc").toMap)
    }

    it("should parse WHERE city=nyc") {
      FutureParser.handle(FutureParser.parseAll(FutureParser.whereClause, "WHERE city=nyc")) should be (Where(Seq("city" -> "nyc").toMap))
    }

    it("should parse a simple query") {
      val expected = Query(WhatWill(Seq("weather")), When(Time.fromSeconds(1371310383), None), Where(Map("city" -> "nyc")))
      val actual = FutureParser("WHAT WILL BE THE weather ON 1371310383 WHERE city=nyc")
      actual should be (expected)
    }
  }
}
