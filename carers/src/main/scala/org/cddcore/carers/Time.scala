package org.cddcore.carers

import org.junit.runner.RunWith
import org.cddcore.engine.Engine
import org.cddcore.engine.tests.CddJunitRunner
import scala.xml.Elem
import org.joda.time.DateTime
import scala.language.implicitConversions
import org.joda.time.DateTimeField

class IllegalDateRangeException(val dateRange: DateRange) extends Exception(dateRange.toString())

case class DateRange(from: DateTime, to: DateTime) {
  if (from.isAfter(to))
    throw new IllegalDateRangeException(this)
}
object DateRange {
  def apply(from: String, to: String): DateRange =
    DateRange(Xmls.asDate(from), Xmls.asDate(to))
}

@RunWith(classOf[CddJunitRunner])
object Time {

  implicit def stringToDate(s: String): DateTime = Xmls.asDate(s)
  implicit def tupleToDate(t: Tuple2[String, String]): DateRange = DateRange(t._1, t._2)

  val dateRangeInDate = Engine[DateTime, DateRange, Boolean]().
    useCase("Date In Range").
    scenario("2010-6-10", "2010-6-8" -> "2010-6-12", "Date in middle").
    expected(true).

    scenario("2010-6-10", "2010-6-8" -> "2010-6-12", "Date on first day").
    expected(true).

    scenario("2010-6-12", "2010-6-8" -> "2010-6-12", "Date on last day").
    expected(true).

    useCase("Returns false if date Not In Range").
    scenario("2010-6-1", "2010-1-1" -> "2010-2-1", "Range in past").
    because((d: DateTime, dr: DateRange) => d.isAfter(dr.to) || d.isBefore(dr.from)).
    expected(false).

    scenario("2010-6-1", "2010-8-1" -> "2010-9-1", "Range in future").
    expected(false).

    scenario("2010-6-10", "2010-6-11" -> "2010-6-12", "Range starts tomorrow").
    expected(false).

    scenario("2010-6-10", "2010-6-8" -> "2010-6-9", "Range ends yesterday").
    expected(false).

    build

  //2013-9-9 is Monday
  val dateRangeCoversWeekOfDate = Engine[DateTime, DateRange, Boolean]().
    useCase("Returns true if the date range includes a day on the same week as the date").
    scenario("2013-9-11", "2013-9-1" -> "2013-9-9", "date range includes the monday, but the date is the wednesday").
    expected(true).
    scenario("2013-9-11", "2013-9-1" -> "2013-9-10", "date range includes the tuesday, but the date is the wednesday").
    expected(true).

    useCase("Returns false if the date range doesnt includes a day on the same week as the date").
    scenario("2013-9-10", "2013-9-1" -> "2013-9-2", "range ends well before date").
    expected(false).
    because((d: DateTime, dr: DateRange) => {
      val dayOfWeek = d.dayOfWeek().get; //Monday = 1, Sunday = 7
      val monday = d.minusDays(dayOfWeek - 1)
      val friday = monday.plusDays(4)
      dr.from.isAfter(friday) | dr.to.isBefore(monday)
    }).
    scenario("2013-9-10", "2013-9-1" -> "2013-9-6", "range ends just before date - friday").
    expected(false).
    scenario("2013-9-10", "2013-9-1" -> "2013-9-8", "range ends just before date - sunday").
    expected(false).
    scenario("2013-9-10", "2013-9-14" -> "2013-9-16", "range starts just after week - saturday").
    expected(false).
    scenario("2013-9-10", "2013-9-16" -> "2013-9-17", "range starts just after week - monday").
    expected(false).
    scenario("2013-9-10", "2013-10-1" -> "2013-10-12", "range starts well after week").
    expected(false).

    build

  def WeeksBetween(startDate: DateTime, current: DateTime): Int =
    {
      val startDayOfWeek = startDate.dayOfWeek().get; //Monday = 1, Sunday = 7
      val startMonday = startDate.minusDays(startDayOfWeek - 1)
      val millis = current.getMillis() - startMonday.getMillis()
      val days = millis / 1000 / 60 / 60 / 24
      val weeks = days / 7
      weeks.toInt
    }
  val weeksActive = Engine[DateTime, DateTime, Int]().
    description("First datetime is the start date. Second is current date. How many weeks between the two dates").
    code((startDate: DateTime, current: DateTime) => 0).
    useCase("If current date is before start date, should be zero").
    scenario("2013-8-10", "2010-4-10", "current day two days before start date").
    expected(0).

    useCase("If current date is in same week, answer is 0 ").
    scenario("2013-10-7", "2013-10-8", "Start date Monday. End date Tuesday").
    expected(0).
    scenario("2013-10-7", "2013-10-13", "Start date Monday. End date Sunday").
    expected(0).
    scenario("2013-10-8", "2013-10-13", "Start date Tuesday. End date Sunday").
    expected(0).

    useCase("The count of the weeks impacted. For example a friday through to monday is a week. (Started wk0, ended wk 1").
    scenario("2013-10-11", "2013-10-14", "Start date Friday / wk 0, End data Monday / wk 1").
    expected(1).
    code((startDate: DateTime, current: DateTime) => WeeksBetween(startDate, current)).
    because((startDate: DateTime, current: DateTime) => WeeksBetween(startDate, current) > 0).

    scenario("2013-10-11", "2013-11-8", "Start date Friday / wk 0, End data Monday / wk 4").
    expected(4).
    code((startDate: DateTime, current: DateTime) => WeeksBetween(startDate, current)).
    build

}