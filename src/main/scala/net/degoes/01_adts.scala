package net.degoes

import net.degoes.events.EventType.SensorUpdated

import java.net.URL
import java.time.Instant
import java.time.temporal.TemporalField

/*
 * Day 1 - 1:32:55
 * INTRODUCTION
 *
 * Functional Design depends heavily on functional data modeling. Functional
 * data modeling is the task of creating precise, type-safe models of a given
 * domain using algebraic data types and generalized algebraic data types.
 *
 * In this section, you'll review basic functional domain modeling.
 */

/**
 * E-COMMERCE - EXERCISE SET 1
 *
 * Consider an e-commerce application that allows users to purchase products.
 */
object credit_card {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a credit card, which must have:
   *
   *  * Number
   *  * Name
   *  * Expiration date
   *  * Security code
   */
  final case class CreditCard(number: String, name: String, expiration: java.time.YearMonth, cvv: SecurityCode)

  sealed abstract case class SecurityCode(value: Int)
  object SecurityCode {
    def fromInt(code: Int): Option[SecurityCode] =
      if (code.toString.length >= 3 && code.toString.length <= 4)
        Some(new SecurityCode(code) {}) // becomes subtype of SecurityCode
      else
        None
  }

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product, which could be a physical product, such as a gallon of milk,
   * or a digital product, such as a book or movie, or access to an event, such
   * as a music concert or film showing.
   */
  sealed trait ProductType
  object ProductType {
    final case class Physical(weight: Double)         extends ProductType
    final case class Digital(url: java.net.URL)       extends ProductType
    final case class Event(date: java.time.LocalDate) extends ProductType
  }

  final case class Product(kind: ProductType, name: String, cost: Double) { self =>
    def increasePrice(amount: Double): Product = self.copy(cost = self.cost + amount)
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product price, which could be one-time purchase fee, or a recurring
   * fee on some regular interval.
   */

  // JDG:
  sealed trait PricingSceme2
  object PricingSceme2 {
    final case class OneTime(amount: Double) extends PricingSceme2
    final case class Subscription(amount: Double, start: java.time.Instant, interval: java.time.Duration) extends PricingSceme2
  }

  // EA:
  final case class PricingScheme(price: Double, recurrence: Recurrence)

  sealed trait Recurrence
  object Recurrence {
    final case object OneTime extends Recurrence
    final case class Recurring(startDate: java.time.LocalDate, interval: TemporalType)
  }

  sealed trait TemporalType {
    val value: Int
  }
  object TemporalType {
    final case class Weekly(override val value: Int) extends TemporalType
    final case class Monthly(override val value: Int) extends TemporalType
    final case class Yearly(override val value: Int) extends TemporalType
  }
}

/**
 * EVENT PROCESSING - EXERCISE SET 3
 *
 * Consider an event processing application, which processes events from both
 * devices, as well as users.
 */
object events {

  /**
   * EXERCISE
   *
   * Refactor the object-oriented data model in this section to a more
   * functional one, which uses only sealed traits and case classes.
   */
  sealed case class Event(id: Int, time: Instant, kind: EventType)
  sealed case class EventPoly[+A](id: Int, time: Instant, kind: A)

  sealed trait EventType
  object EventType {
    private case class User(userName: String) extends EventType
    private case class Device(deviceId: Int) extends EventType

    case class SensorUpdated(override val deviceId: Int, reading: Option[Double]) extends Device(deviceId)
    case class DeviceActivated(override val deviceId: Int) extends Device(deviceId)
    case class UserPurchase(override val userName: String, item: String, price: Double) extends User(userName)
    case class UserAccountCreated(override val userName: String) extends User(userName)
  }

  val x: Event = Event(5412, Instant.now(), SensorUpdated(1, Some(21.5)))
  val y: EventPoly[SensorUpdated] = EventPoly[SensorUpdated](5413, Instant.now, SensorUpdated(1, Some(21.5)))
}

/**
 * DOCUMENT EDITING - EXERCISE SET 4
 *
 * Consider a web application that allows users to edit and store documents
 * of some type (which is not relevant for these exercises).
 */
object documents {
  final case class UserId(identifier: String)
  final case class DocId(identifier: String)
  final case class DocContent(body: String)

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create a simplified but somewhat
   * realistic model of a Document.
   */
  type Document

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create a model of the access
   * type that a given user might have with respect to a document. For example,
   * some users might have read-only permission on a document.
   */
  type AccessType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create a model of the
   * permissions that a user has on a set of documents they have access to.
   * Do not store the document contents themselves in this model.
   */
  type DocPermissions
}

/**
 * BANKING - EXERCISE SET 5
 *
 * Consider a banking application that allows users to hold and transfer money.
 */
object bank {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a customer at a bank.
   */
  type Customer

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of an account
   * type. For example, one account type allows the user to write checks
   * against a given currency. Another account type allows the user to earn
   * interest at a given rate for the holdings in a given currency.
   */
  type AccountType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a bank
   * account, including details on the type of bank account, holdings, customer
   * who owns the bank account, and customers who have access to the bank account.
   */
  type Account
}

/**
 * STOCK PORTFOLIO - GRADUATION PROJECT
 *
 * Consider a web application that allows users to manage their portfolio of investments.
 */
object portfolio {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * exchange. Ensure there exist values for NASDAQ and NYSE.
   */
  type Exchange

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of a currency
   * type.
   */
  type CurrencyType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * symbol. Ensure there exists a value for Apple's stock (APPL).
   */
  type StockSymbol

  /**
   * EXERCISE 4
   *
   * Using only sealed traits and case classes, develop a model of a portfolio
   * held by a user of the web application.
   */
  type Portfolio

  /**
   * EXERCISE 5
   *
   * Using only sealed traits and case classes, develop a model of a user of
   * the web application.
   */
  type User

  /**
   * EXERCISE 6
   *
   * Using only sealed traits and case classes, develop a model of a trade type.
   * Example trade types might include Buy and Sell.
   */
  type TradeType

  /**
   * EXERCISE 7
   *
   * Using only sealed traits and case classes, develop a model of a trade,
   * which involves a particular trade type of a specific stock symbol at
   * specific prices.
   */
  type Trade
}
