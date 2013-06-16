package com.mosesn.fql

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers

import com.twitter.util.Time

class QueryExecutorSpec extends FunSpec with ShouldMatchers {
  describe("QueryExecutor") {
    it("should hand back the time") {
      val ts = Time.fromSeconds(Time.now.inSeconds)
      val query = Query(WhatWill(Seq("time")), When(ts, None), Where(Map.empty))
      val expected = Seq(ts.toString)
      DefaultQueryExecutor(query).get should be (expected)
    }

    /*
    it("should hand back the weather") {

    }
     */
  }
}
