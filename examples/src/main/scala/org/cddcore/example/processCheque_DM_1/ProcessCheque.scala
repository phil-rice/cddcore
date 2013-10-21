package org.cddcore.example.processCheque_DM_1

import scala.language.implicitConversions
import org.joda.time.DateTime
import org.cddcore.engine._
import org.junit.runner.RunWith
import org.cddcore.engine.tests._

//This is compatible with 
//	<dependency>
//		<groupId>org.cddcore</groupId>
//		<artifactId>engine_2.10</artifactId>
//		<version>1.2.0</version>
//	</dependency>

case class World(date: DateTime, thisBank: BankId, customerIdToCustomer: (CustomerId) => Customer, acceptedBanks: List[BankId] = List(BankId.hsbc, BankId.rbs, BankId.thisBank));

case class BankId(id: String)
object BankId {
  def thisBank = BankId("this")
  def hsbc = BankId("HSBC")
  def rbs = BankId("RBS")
  def dodgyBank = BankId("DodgyBank")
}

object GBP {
  implicit def intToGBP(i: Int) = GBP(i, 0)
  implicit def GBPToDouble(g: GBP) = g.pounds + g.pence / 100.0
}
case class GBP(pounds: Integer, pence: Integer)

case class CustomerId(id: String, bank: BankId)
case class Customer(id: CustomerId, balance: GBP, overdraftLimit: GBP, premiumCustomer: Boolean)
case class Cheque(refNo: String, from: CustomerId, to: CustomerId, date: DateTime, amount: GBP)

object Message {
  implicit def stringToMessage(s: String) = Message(s)
  implicit def tupleToMessage(t: Tuple2[String, _]) = Message(t._1, t._2)
}
case class Message(pattern: String, keys: Any*)
case class ProcessChequeResult(pay: Boolean, message: Message)

object ProcessChequeTestMother {
  import GBP._

  val dodgyDaveId = CustomerId("12", BankId.thisBank)
  val dodgyDaveAtDodgyBankId = CustomerId("12", BankId.dodgyBank)
  val dodgyDave = Customer(dodgyDaveId, 100, 0, false)

  val richRogerId = CustomerId("34", BankId.thisBank)
  val richRoger = Customer(richRogerId, 10000, 4000, false)
  val richRogerAtHsbcId = CustomerId("34", BankId.hsbc)

  val today = DateTime.parse("2013-9-1")
  val world = World(today, BankId.thisBank, Map(dodgyDaveId -> dodgyDave, richRogerId -> richRoger))

}

@RunWith(classOf[CddJunitRunner])
object ProcessCheque {
  import ProcessChequeTestMother._
  import GBP._
  val processCheque = Engine[World, Cheque, ProcessChequeResult]().
    description("This engine works out whether to pay a cheque that has been presented to the bank specified in the world").
    code((w: World, c: Cheque) => ProcessChequeResult(false, "processCheque.defaultResult.shouldntHappen")).

    useCase("Cheques that are for a different bank should be rejected").
    scenario(world, Cheque("1", richRogerAtHsbcId, richRogerId, today, 1000), "One thousand pounds from rich roger at HSBC to rich roger at this bank. But the 'FromBank' isn't this bank").
    expected(ProcessChequeResult(false, "processCheque.reject.fromBankNotThisBank")).
    because((w: World, c: Cheque) => c.from.bank != w.thisBank).

    useCase("Cheques that are to a bank not on the white list should be rejected").
    scenario(world, Cheque("1", dodgyDaveId, dodgyDaveAtDodgyBankId, today, 50), "Dodgy Dave is moving half his funds to a bank that isn't on the accepted list").
    expected(ProcessChequeResult(false, ("processCheque.reject.toBank.notInWhiteList", BankId.dodgyBank))).
    because((w: World, c: Cheque) => !w.acceptedBanks.contains(c.to.bank)).
    //
    useCase("Cheques that will take the customer over the overdraft limit will should be rejected").
    scenario(world, Cheque("1", dodgyDaveId, richRogerId, today, 110), "Dodgy Dave sending more money than he has").
    expected(ProcessChequeResult(false, "processCheque.reject.noOverdraft")).
    because((w: World, c: Cheque) => {
      val customer = w.customerIdToCustomer(c.from)
      c.amount >= customer.balance && customer.overdraftLimit == GBP(0, 0)
    }).
    //
    scenario(world, Cheque("1", richRogerId, richRogerAtHsbcId, today, 15000), "Rich Roger sending more money than he has, taking him over his limit").
    expected(ProcessChequeResult(false, "processCheque.reject.exceedsOverdraftLimit")).
    because((w: World, c: Cheque) => {
      val customer = w.customerIdToCustomer(c.from)
      c.amount >= customer.balance + customer.overdraftLimit
    }).
    //
    useCase("Cheques that are to to customers in an accepted bank, when the cheque writer has sufficient funds, should be allowed").
    scenario(world, Cheque("1", dodgyDaveId, richRogerId, today, 50), "Dodgy Dave sending an OK cheque to someone in this bank").
    expected(ProcessChequeResult(true, "processCheque.accept")).
    because((w: World, c: Cheque) => w.acceptedBanks.contains(c.to.bank)).

    scenario(world, Cheque("1", dodgyDaveId, richRogerAtHsbcId, today, 50), "Dodgy Dave sending an OK cheque to someone in an accepted bank").
    expected(ProcessChequeResult(true, "processCheque.accept")).

    build

  def main(args: Array[String]) {
    println(processCheque(world, Cheque("1", dodgyDaveId, richRogerAtHsbcId, today, 50)))
  }

}