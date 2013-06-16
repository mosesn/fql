package com.mosesn.fql

trait QueryExecutor {
  def apply(query: Query): Option[Seq[String]]
}

trait BuiltinQueryExecutor extends QueryExecutor {
  def apply(query: Query): Option[Seq[String]] = query match {
    case Query(WhatWill(columns), When(time, maybeDuration), Where(constraints)) => columns.find(_ == "time").map(_ => Seq(time.toString))
  }
}

trait AggregatingQueryExecutor extends QueryExecutor {
  val executors: Seq[QueryExecutor]

  def apply(query: Query): Option[Seq[String]] = executors.foldLeft[Option[Seq[String]]](None)((acc, executor) => acc orElse executor(query))
}

object DefaultQueryExecutor extends AggregatingQueryExecutor {
  val executors: Seq[QueryExecutor] = Seq(new BuiltinQueryExecutor{})
}
