package org.cddcore.example.processCheque_DM_2
import scala.language.implicitConversions
import org.joda.time.DateTime
import org.cddcore.engine._
import org.junit.runner.RunWith
import org.cddcore.engine.tests._
import org.cddcore.engine.LoggerDisplay
import org.joda.time.format.DateTimeFormat

//This is compatible with 
//	<dependency>
//		<groupId>org.cddcore</groupId>
//		<artifactId>engine_2.10</artifactId>
//		<version>1.2.0</version>
//	</dependency>

case class World(date: DateTime, thisBank: BankId, customerIdToCustomer: (CustomerId) => Customer, acceptedBanks: List[BankId] = List(BankId.hsbc, BankId.rbs, BankId.thisBank)) extends LoggerDisplay {
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
case class Message(pattern: String, keys: Any*)extends LoggerDisplay {
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
  val dodgyDave = Customer(dodgyDaveId, 100, 0, false)

  val richRogerId = CustomerId("34", BankId.thisBank)
  val richRoger = Customer(richRogerId, 10000, 4000, false)
  val richRogerAtHsbcId = CustomerId("34", BankId.hsbc)

  val today = DateTime.parse("2013-9-1")
  val world = World(today, BankId.thisBank, Map(dodgyDaveId -> dodgyDave, richRogerId -> richRoger))

}

case class ChequeSituation(world: World, cheque: Cheque) extends LoggerDisplay {
  import GBP._

  lazy val customer = world.customerIdToCustomer(cheque.from)
  lazy val customerWouldBeOverDrawn = cheque.amount > customer.balance
  lazy val customerHasNoOverdraftLimit = customer.overdraftLimit == GBP(0, 0)
  lazy val customerWouldExceedOverdraftLimit = cheque.amount >= customer.balance + customer.overdraftLimit

  def loggerDisplay(dp: LoggerDisplayProcessor) = s"(${ dp(world)}, ${dp(cheque)})"

}

@RunWith(classOf[CddJunitRunner])
object ProcessCheque {
  import ProcessChequeTestMother._
  import GBP._
  implicit def tupleToChequeSituation(t: (World, Cheque)) = ChequeSituation(t._1, t._2)

  val processCheque = Engine[ChequeSituation, ProcessChequeResult]().
    code((s: ChequeSituation) => ProcessChequeResult(false, "processCheque.defaultResult.shouldntHappen")).
    description("This engine works out whether to pay a cheque that has been presented to the bank specified in the world").

    useCase("Cheques that are for a different bank should be rejected").
    scenario((world, Cheque("1", richRogerAtHsbcId, richRogerId, today, 1000)), "One thousand pounds from rich roger at HSBC to rich roger at this bank. But the 'FromBank' isn't this bank").
    expected(ProcessChequeResult(false, "processCheque.reject.fromBankNotThisBank")).
    because((s: ChequeSituation) => s.cheque.from.bank != s.world.thisBank).

    useCase("Cheques that are to a bank not on the white list should be rejected").
    scenario((world, Cheque("1", dodgyDaveId, dodgyDaveAtDodgyBankId, today, 50)), "Dodgy Dave is moving half his funds to a bank that isn't on the accepted list").
    expected(ProcessChequeResult(false, ("processCheque.reject.toBank.notInWhiteList", BankId.dodgyBank))).
    because((s: ChequeSituation) => !s.world.acceptedBanks.contains(s.cheque.to.bank)).
    //
    useCase("Cheques that will take the customer over the overdraft limit will should be rejected").
    scenario((world, Cheque("1", dodgyDaveId, richRogerId, today, 110)), "Dodgy Dave sending more money than he has").
    expected(ProcessChequeResult(false, "processCheque.reject.noOverdraft")).
    because((s: ChequeSituation) => s.customerWouldBeOverDrawn && s.customerHasNoOverdraftLimit).
    //
    scenario((world, Cheque("1", richRogerId, richRogerAtHsbcId, today, 15000)), "Rich Roger sending more money than he has, taking him over his limit").
    expected(ProcessChequeResult(false, "processCheque.reject.exceedsOverdraftLimit")).
    because((s: ChequeSituation) => s.customerWouldExceedOverdraftLimit).
    //
    useCase("Cheques that are to to customers in an accepted bank, when the cheque writer has sufficient funds, should be allowed").
    scenario((world, Cheque("1", dodgyDaveId, richRogerId, today, 50)), "Dodgy Dave sending an OK cheque to someone in this bank").
    expected(ProcessChequeResult(true, "processCheque.accept")).
    because((s: ChequeSituation) => s.world.acceptedBanks.contains(s.cheque.to.bank)).

    scenario((world, Cheque("1", dodgyDaveId, richRogerAtHsbcId, today, 50)), "Dodgy Dave sending an OK cheque to someone in an accepted bank").
    expected(ProcessChequeResult(true, "processCheque.accept")).

    build

  def main(args: Array[String]) {
    println(processCheque(world, Cheque("1", dodgyDaveId, richRogerAtHsbcId, today, 50)))
  }

}