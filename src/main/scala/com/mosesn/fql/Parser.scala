package com.mosesn.fql

import scala.util.parsing.combinator.JavaTokenParsers
import com.twitter.util.{Time, Duration}

object FutureParser extends JavaTokenParsers {
  def apply(string: String): Query = parseAll(fql, string) match {
    case Success(result, _) => result
    case failure : NoSuccess => throw new Exception("Not a valid query string")
  }

  lazy val fql: Parser[Query] = selectClause ~ whenClause ~ whereClause ^^ {
    case a ~ b ~ c => Query(a, b, c)
  }

  lazy val selectClause: Parser[WhatWill] = select ~> selectors ^^ {
    case strings => WhatWill(strings)
  }


  lazy val whereClause: Parser[Where] = where ~> keyValues ^^ (Where(_))

  lazy val where: Parser[String] = "WHERE"

  lazy val keyValues: Parser[Map[String, String]] = "" ~> rep1(keyValue) ^^ (_.toMap)

  lazy val keyValue: Parser[(String, String)] = letters ~ '=' ~ letters ^^ {
    case key ~ _ ~ value => (key, value)
  }

  lazy val letters: Parser[String] = rep1(acceptIf(char => char.isLetter)(char => "Char %s was not a letter." format char.toString)) ^^ (_.mkString)

  lazy val fromClause: Parser[From] = from ~> letters ^^ (From(_))

  lazy val from: Parser[String] = "FROM"

  lazy val whenClause: Parser[When] = on ~> timespace ^^ When.tupled

  lazy val on: Parser[String] = "ON"

  lazy val timespace: Parser[(Time, Option[Duration])] = timestamp ~ opt(untilClause) ^^ {
    case start ~ maybeEnd => (start, maybeEnd map (end => end - start))
  }

  lazy val timestamp: Parser[Time] = int ^^ (Time.fromSeconds(_))

  lazy val long: Parser[Long] = wholeNumber ^^ (_.toLong)

  lazy val int: Parser[Int] = wholeNumber ^^ (_.toInt)

  lazy val untilClause: Parser[Time] = until ~> timestamp

  lazy val until: Parser[String] = "UNTIL"

  lazy val selectors: Parser[Seq[String]] = star | columns

  lazy val star: Parser[Seq[String]] = "*" ^^ (_ => Seq.empty)

  // TODO MN: stupid hack :(
  lazy val columns: Parser[Seq[String]] = "" ~> rep1sep(letters, ",")

  lazy val select: Parser[String] = "WHAT WILL BE THE"

  def handle[T](parser: ParseResult[T]): T = parser match {
    case Success(result, _) => result
    case failure => {
      println(failure)
      throw new Exception("damnit")
    }
  }
}
