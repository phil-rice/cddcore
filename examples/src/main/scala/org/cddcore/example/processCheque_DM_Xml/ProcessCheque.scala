package org.cddcore.example.processCheque_DM_Xml

import scala.language.implicitConversions
import org.joda.time.DateTime
import org.cddcore.engine._
import org.junit.runner.RunWith
import org.cddcore.engine.tests._
import org.cddcore.engine.LoggerDisplay
import org.joda.time.format.DateTimeFormatter
import org.joda.time.format.DateTimeFormat
import scala.xml.Elem
import scala.xml.NodeSeq

//This is compatible with 
//	<dependency>
//		<groupId>org.cddcore</groupId>
//		<artifactId>engine_2.10</artifactId>
//		<version>1.2.0</version>
//	</dependency>

case class MapWithDefault[K, V](map: Map[K, V], default: V) extends Function[K, V] {
  def apply(k: K) = map.getOrElse(k, default)
}

case class World(date: DateTime, thisBank: BankId, customerIdToCustomer: (CustomerId) => Elem, acceptedBanks: List[BankId] = List(BankId.hsbc, BankId.rbs, BankId.thisBank)) extends LoggerDisplay {
  def loggerDisplay(dp: LoggerDisplayProcessor) = s"World(${Dates.dateToString(date)}"
}

case class BankId(id: String)

object BankId {
  def thisBank = BankId("this")
  def hsbc = BankId("HSBC")
  def rbs = BankId("RBS")
  def dodgyBank = BankId("DodgyBank")
}

object GBP {
  implicit def intToGBP(i: Int) = GBP(i, 0)
  implicit def stringToGBP(s: String) = { val d: Double = java.lang.Double.parseDouble(s); val pounds = Math.floor(d).toInt; GBP(pounds, (d - pounds).toInt) }
  implicit def GBPToDouble(g: GBP) = g.pounds + g.pence / 100.0
}
case class GBP(pounds: Integer, pence: Integer)

case class CustomerId(id: String, bank: BankId) extends LoggerDisplay {
  def loggerDisplay(dp: LoggerDisplayProcessor) = s"CustomerId(${bank.id}/$id)"
}
case class Customer(id: CustomerId, balance: GBP, overdraftLimit: GBP, premiumCustomer: Boolean)

case class Cheque(refNo: String, from: CustomerId, to: CustomerId, date: DateTime, amount: GBP) extends LoggerDisplay {
  def loggerDisplay(dp: LoggerDisplayProcessor) = s"Cheque(${refNo}, from=${dp(from)}, to=${dp(to)}, ${Dates.dateToString(date)}, amount=$amount"
}

object Message {
  implicit def stringToMessage(s: String) = Message(s)
  implicit def tupleToMessage(t: Tuple2[String, _]) = Message(t._1, t._2)
}
case class Message(pattern: String, keys: Any*) extends LoggerDisplay {
  def loggerDisplay(dp: LoggerDisplayProcessor) = s"Message($pattern${keys.mkString(" ", ", ", "")})"
}
case class ProcessChequeResult(pay: Boolean, message: Message)
object Dates {
  def dateToString(d: DateTime) = DateTimeFormat.forPattern("yyyy-MM-dd").print(d)
}

object ProcessChequeTestMother {
  import GBP._

  val dodgyDaveId = CustomerId("12", BankId.thisBank)
  val dodgyDaveAtDodgyBankId = CustomerId("12", BankId.dodgyBank)
  val dodgyDave = <Customer>
                    <Id>12</Id>
                    <Bank>this</Bank>
                    <Balance>100.0</Balance>
                    <OverdraftLimit>0</OverdraftLimit>
                    <PremiumCustomer>false</PremiumCustomer>
                  </Customer>

  val richRogerId = CustomerId("34", BankId.thisBank)
  val richRoger = <Customer>
                    <Id>34</Id>
                    <Bank>this</Bank>
                    <Balance>10000.0</Balance>
                    <OverdraftLimit>4000.0</OverdraftLimit>
                    <PremiumCustomer>true</PremiumCustomer>
                  </Customer>
  val richRogerAtHsbcId = CustomerId("123", BankId.hsbc)

  val today = DateTime.parse("2013-9-1")
  val world = World(today, BankId.thisBank, MapWithDefault(Map(dodgyDaveId -> dodgyDave, richRogerId -> richRoger), <NoCustomerRecord/>))

  def cheque(ref: String, from: CustomerId, to: CustomerId, date: DateTime, amount: Double) =
    {
      val e =
        <cheque>
          <From><Bank>{ from.bank.id }</Bank><Id>{ from.id }</Id></From>
          <To><Bank>{ to.bank.id }</Bank><Id>{ to.id }</Id></To>
          <Amount> { amount }</Amount>
          <Date>{ Dates.dateToString(date) }</Date>
        </cheque>
      e
    }
}

case class ChequeSituation(world: World, cheque: Elem) extends XmlSituation {
  import GBP._
  import Xml._
  def gbp = (n: NodeSeq) => Some(stringToGBP(n.text))
  def bankId = (n: NodeSeq) => Some(BankId(n.text))

  lazy val chequeFromBank = xml(cheque) \ "From" \ "Bank" \ bankId
  lazy val chequeFromId = xml(cheque) \ "From" \ "Id" \ string
  lazy val chequeFrom = CustomerId(chequeFromId(), chequeFromBank())

  lazy val chequeToBank = xml(cheque) \ "To" \ "Bank" \ bankId
  lazy val chequeToId = xml(cheque) \ "To" \ "Id" \ string
  lazy val chequeTo = CustomerId(chequeToId(), chequeToBank())
  lazy val chequeAmount = xml(cheque) \ "Amount" \ gbp

  lazy val customer: Elem = world.customerIdToCustomer(chequeFrom)
  lazy val customerBalance = xml(customer) \ "Balance" \gbp
  lazy val customerOverdraftLimit = xml(customer) \ "OverdraftLimit"\gbp

  lazy val customerWouldBeOverDrawn = chequeAmount() > customerBalance()
  lazy val customerHasNoOverdraftLimit = customerOverdraftLimit() == GBP(0, 0)
  lazy val customerWouldExceedOverdraftLimit = chequeAmount() >= customerBalance() + customerOverdraftLimit()

  def loggerDisplay(dp: LoggerDisplayProcessor) = s"(${dp(world)}, ${dp(cheque)})"

}
class ProcessChequeXml

@RunWith(classOf[CddJunitRunner])
object ProcessChequeXml {
  import ProcessChequeTestMother._
  import GBP._
  implicit def tupleToChequeSituation(t: (World, Elem)) = ChequeSituation(t._1, t._2)

  val processCheque = Engine[ChequeSituation, ProcessChequeResult]().
    code((s: ChequeSituation) => ProcessChequeResult(false, "processCheque.defaultResult.shouldntHappen")).
   description("This engine works out whether to pay a cheque that has been presented to the bank specified in the world").

    useCase("Cheques that are for a different bank should be rejected").
    scenario((world, cheque("1", richRogerAtHsbcId, richRogerId, today, 1000)), "One thousand pounds from rich roger at HSBC to rich roger at this bank. But the 'FromBank' isn't this bank").
    expected(ProcessChequeResult(false, "processCheque.reject.fromBankNotThisBank")).
    because((s: ChequeSituation) => s.chequeFromBank() != s.world.thisBank).

    useCase("Cheques that are to a bank not on the white list should be rejected").
    scenario((world, cheque("1", dodgyDaveId, dodgyDaveAtDodgyBankId, today, 50)), "Dodgy Dave is moving half his funds to a bank that isn't on the accepted list").
    expected(ProcessChequeResult(false, ("processCheque.reject.toBank.notInWhiteList", BankId.dodgyBank))).
    because((s: ChequeSituation) => {
      val c = s.chequeToBank()
      !s.world.acceptedBanks.contains(c)
    }).

    //
    useCase("Cheques that will take the customer over the overdraft limit will should be rejected").
    scenario((world, cheque("1", dodgyDaveId, richRogerId, today, 110)), "Dodgy Dave sending more money than he has").
    expected(ProcessChequeResult(false, "processCheque.reject.noOverdraft")).
    because((s: ChequeSituation) => s.customerWouldBeOverDrawn && s.customerHasNoOverdraftLimit).
    //
    scenario((world, cheque("1", richRogerId, richRogerAtHsbcId, today, 15000)), "Rich Roger sending more money than he has, taking him over his limit").
    expected(ProcessChequeResult(false, "processCheque.reject.exceedsOverdraftLimit")).
    because((s: ChequeSituation) => s.customerWouldExceedOverdraftLimit).
    //
    useCase("Cheques that are to to customers in an accepted bank, when the cheque writer has sufficient funds, should be allowed").
    expected(ProcessChequeResult(true, "processCheque.accept")).
    scenario((world, cheque("1", dodgyDaveId, richRogerId, today, 50)), "Dodgy Dave sending an OK cheque to someone in this bank").
    because((s: ChequeSituation) => s.world.acceptedBanks.contains(s.chequeToBank())).

    scenario((world, cheque("1", dodgyDaveId, richRogerAtHsbcId, today, 50)), "Dodgy Dave sending an OK cheque to someone in an accepted bank").

    build

  def main(args: Array[String]) {
    println(processCheque(world, cheque("1", dodgyDaveId, richRogerAtHsbcId, today, 50)))
  }

}