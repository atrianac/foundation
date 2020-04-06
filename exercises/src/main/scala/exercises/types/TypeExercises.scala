package exercises.types

import java.time.Instant
import java.util.UUID

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.PosInt
import exercises.sideeffect.IOExercises.IO
import exercises.types.Card._
import exercises.types.TypeExercises.Comparision.{EqualsTo, GreaterThan, LessThan}
import exercises.types.TypeExercises.OrderStatus.{Checkout, Submitted}
import exercises.types.TypeExercises.StreetItem.{PostCode, StreetName, StreetNumber}

// You can run and print things here:
object TypeApp extends App {
  import TypeExercises._

  println(boolean.cardinality)
}

object TypeExercises {


  //
  //
  //
  // IO[C, Either[E, T]]
  //

  ////////////////////////
  // 1. Misused types
  ////////////////////////

  // 1a. Implement `compareChar` that indicates if `c1` is smaller, equal to or larger than `c2`
  // such as compareChar('a', 'c') == -1
  //         compareChar('c', 'c') ==  0
  //         compareChar('c', 'a') ==  1
  // What is wrong with this function? How could you improve it?
  def compareChar(c1: Char, c2: Char): Int =
    if(c1 < c2) -1
    else if (c1 == c2) 0
    else 1

  sealed trait Comparision
  object Comparision {
    case object LessThan extends Comparision
    case object EqualsTo extends  Comparision
    case object GreaterThan extends Comparision
  }

  def compareCharTypeCheck(c1: Char, c2: Char): Comparision =
    if(c1 < c2) LessThan
    else if (c1 == c2) EqualsTo
    else GreaterThan

  // 1b. Implement `mostRecentBlogs` that returns the `n` most recent blog posts
  // such as mostRecentBlogs(1)(List(
  //   BlogPost(1,First blog,2019-09-18T16:21:06.681768Z)
  //   BlogPost(23,Thoughts of the day,2019-09-21T08:14:06.702836Z)
  // )) == List(BlogPost(23,Thoughts of the day,2019-09-21T08:14:06.702836Z))
  // What is wrong with this function? How could you improve it?
  case class BlogPost(id: String, title: String, createAt: String)

  def mostRecentBlogs(n: Int)(blogs: List[BlogPost]): List[BlogPost] =
    blogs.sortBy(_.createAt).take(n)

  case class BlogPostTypeCheck(id: String, title: String, createAt: Instant)

  def mostRecentBlogsTypeCheck(n: Int)(blogs: List[BlogPostTypeCheck]): List[BlogPostTypeCheck] =
    blogs.sortBy(_.createAt).take(n)


  // 1c. Implement `User#address` that returns the full address for a User (e.g. to send a parcel)
  // such as User("John Doe", Some(108), Some("Cannon Street"), Some("EC4N 6EU")) == "108 Canon Street EC4N 6EU"
  // What is wrong with this function? How could you improve it?
  case class User(name: String, streetNumber: Option[Int], streetName: Option[String], postCode: Option[String]) {
    def address: String = {
      val stNumber = streetNumber.getOrElse("")
      val stName = streetName.getOrElse("")
      val stPostCode = streetName.getOrElse("")

      stNumber + stName + stPostCode
    }
  }

  trait StreetItem
  object StreetItem {
    case class StreetNumber(value: Int) extends StreetItem
    case class StreetName(value: String) extends StreetItem
    case class PostCode(value: String) extends  StreetItem
  }

  case class UserTypeCheck(name: String, streetNumber: Option[StreetNumber], streetName: Option[StreetName], postCode: Option[PostCode]) {
    def address: String = {
      val stNumber = streetNumber.fold("")(_.value.toString)
      val stName = streetName.fold("")(_.value)
      val stPostCode = streetName.fold("")(_.value)

      stNumber + stName + stPostCode
    }
  }

  // 1d. Implement `Invoice#discountFirstItem` that returns a new invoice with the first item discounted.
  // For example, discountFirstItem(0.3) would apply a 30% discount.
  // What is wrong with this function? How could you improve it?
  case class InvoiceItem(id: String, quantity: Int, price: Double)
  // An invoice must have at least one item.
  case class Invoice(id: String, items: List[InvoiceItem]) {
    def discountFirstItem(discountPercent: Double): Invoice = items match {
      case ::(fstItem, tail) => this.copy(items = fstItem.copy(price = discountPercent * fstItem.price) :: tail)
      case Nil => this
    }
  }

  // case class EndavaEmpleados(id: number, name: String)
  //
  // case class EndavaNoEmpleados(id: number, name: String)
  //
  // type Asistentes = EndavaEmpleados | EndavaNoEmpleados
  // case class Conferencia(asistentes: List[Asistentes])

  // List(Alejandro, Javier, Farid, Danilo, Juan, Juan)
  // List.head instaceOf

  // List.head == Asistentes
  // HList.head  == EndavaNoEmpleados
  //

  case class InvoiceTypeCheck(id: String, items: NonEmptyList[InvoiceItem]) {

    def discountFirstItem(discountPercent: Double): InvoiceTypeCheck = {
      val fstItem = this.items.head;
      val itemDiscount = fstItem.copy(price = fstItem.price * discountPercent)
      InvoiceTypeCheck(this.id, NonEmptyList(itemDiscount, items.tail))
    }
  }

  // 1e. Implement `createTicket` that instantiates a Ticket with 0 story point,
  // a random ticket id (see `genTicketId`) and the current time (see `readNow`).
  // What is wrong with this function? How could you improve it?
  def createTicket(title: String): IO[Ticket] = for {
      ticketId <- genTicketId
      currentTime <- readNow
  } yield (Ticket(ticketId, title, 0, currentTime))

  def genTicket(title: String): IO[Ticket] = for {
    ticketId <- genTicketId
    currentTime <- readNow
  } yield (createTicket(ticketId, title, currentTime))


  def createTicket(ticketId: TicketId, title: String, createdAt: Instant): Ticket =
    Ticket(
      id = ticketId,
      title = title,
      storyPoints = 0,
      createdAt = createdAt
    )

  def genTicketId: IO[TicketId] = IO.effect(TicketId(UUID.randomUUID()))
  def readNow: IO[Instant]      = IO.effect(Instant.now())


  case class TicketId(value: UUID)
  // Tuple4[TicketId, String, Int, Instant]
  case class Ticket(id: TicketId, title: String, storyPoints: Int, createdAt: Instant)

  ////////////////////////
  // 2. Data Encoding
  ////////////////////////

  // 2a. Create types that encode the following business requirements:
  // An order contains an order id (UUID), a created timestamp (Instant), an order status, and a basket of items.
  // An order status is either a draft, checkout, submitted or delivered.
  // An item consists of an item id (UUID), a quantity and a price.
  // A basket can be empty in draft, otherwise it must contain at least one item.
  // When an order is in checkout, it may have a delivery address.
  // When an order is in submitted, it must have a delivery address and a submitted timestamp (Instant).
  // When an order is in delivered, it must have a delivery address, a submitted and delivered timestamps (Instant).
  // An address consists of a street number and a post code.
  case class Order(orderId: OrderId, createdTime: CreatedTime, orderStatus: OrderStatus)

  case class OrderId(uuid: UUID)
  case class CreatedTime(instant: Instant)

  trait OrderStatus
  object OrderStatus {
    case class Draft(basket: List[Item])                                                             extends OrderStatus
    case class Checkout(basket: NonEmptyList[Item], deliveryAddress: Option[Address])                extends OrderStatus
    case class Submitted(basket: NonEmptyList[Item], deliveryAddress: Address, submittedAt: Instant) extends OrderStatus
    case class Delivered(basket: NonEmptyList[Item],
                         deliveryAddress: Address,
                         submittedAt: Instant,
                         deliveredAt: Instant)
      extends OrderStatus
    case class Cancelled(previousState: Either[Checkout, Submitted], cancelledAt: Instant) extends OrderStatus
  }


  case class Item(itemId: ItemId, quantity: Quantity, price: Price)
  case class ItemId(uuid: UUID)
  case class Quantity(value: Int)
  case class Price(value: Double)

  case class Address(streetNumber: StreetNumber, postCode: PostCode)
  case class StreetNumber(value: Int)
  case class PostCode(value: String)

  trait OrderOperationError
  case class OrderTransitionStatusError(srcStatus: OrderStatus, msg: String) extends OrderOperationError
  case class MandatoryFieldError(msg: String) extends OrderOperationError

  // 2b. Implement `submit` which encodes the order transition between `Checkout` to `Submitted`.
  // Verify all pre and post conditions are satisfied and if not encode the errors in an ADT.
  // What parameters should submit take?
  def submit(order: Order): Either[OrderOperationError, Order] =
    order.orderStatus match {
      case Checkout(basket, deliveryAddress) => {
        deliveryAddress match {
          case Some(value) => Right(order.copy(orderStatus = Submitted(basket, value, Instant.now())))
          case None => Left(MandatoryFieldError("Field deliveryAddress is mandatory submitting the order"))
        }
      }
      case _ => Left(OrderTransitionStatusError(order.orderStatus, "Cannot submit the order, incorrect status"))
    }

  // 2c. Implement `deliver` which encodes the order transition between `Submitted` to `Delivered` status.
  // Verify all pre and post conditions are satisfied and, if not, encode the errors in an ADT.
  // You may need to modify your encoding to eliminate runtime errors.
  def deliver = ???

  // 2d. Add a cancelled status.
  // An order can be cancelled only if it has a `Checkout` or `Submitted` status.
  // A cancelled order must have a cancelled timestamp (Instant).

  // 2e. Implement `cancel` which encodes the order transition between `Checkout` or `Submitted` to `Cancelled` status.
  // Verify all pre and post conditions are satisfied and, if not, encode the errors in an ADT.
  // You may need to modify your encoding to eliminate runtime errors.

  ////////////////////////
  // 3. Cardinality
  ////////////////////////

  val boolean: Cardinality[Boolean] = new Cardinality[Boolean] {
    def cardinality: Card = Constant(2)
  }

  val int: Cardinality[Int] = new Cardinality[Int] {
    def cardinality: Card = Constant(2) ^ Constant(32)
  }

  // 3a. How many possible values exist of type Any?
  val any: Cardinality[Any] = new Cardinality[Any] {
    def cardinality: Card = Inf
  }

  // 3b. How many possible values exist of type Nothing?
  val nothing: Cardinality[Nothing] = new Cardinality[Nothing] {
    def cardinality: Card = Constant(0)
  }

  // 3c. How many possible values exist of type Unit?
  val unit: Cardinality[Unit] = new Cardinality[Unit] {
    def cardinality: Card = Constant(1)
  }

  val ioUnit: Cardinality[IO[Unit]] = new Cardinality[IO[Unit]] {
    def cardinality: Card = Inf
  }

  // 3d. How many possible values exist of type Byte?
  val bool: Cardinality[Byte] = new Cardinality[Byte] {
    def cardinality: Card = Constant(2) ^ Constant(8)
  }

  // 3e. How many possible values exist of type Option[Boolean]?
  val optBoolean: Cardinality[Option[Boolean]] = new Cardinality[Option[Boolean]] {
    def cardinality: Card = Constant(1) + boolean.cardinality
  }

  // 3f. How many possible values exist of type IntOrBoolean?
  val intOrBoolean: Cardinality[IntOrBoolean] = new Cardinality[IntOrBoolean] {
    def cardinality: Card = int.cardinality + boolean.cardinality
  }

  sealed trait IntOrBoolean
  object IntOrBoolean {
    case class AnInt(value: Int)        extends IntOrBoolean
    case class ABoolean(value: Boolean) extends IntOrBoolean
  }

  // 3g. How many possible values exist of type IntAndBoolean?
  val intAndBoolean: Cardinality[IntAndBoolean] = new Cardinality[IntAndBoolean] {
    def cardinality: Card = int.cardinality * boolean.cardinality
  }

  case class IntAndBoolean(i: Int, b: Boolean)

  ///////////////////////
  // GO BACK TO SLIDES
  ///////////////////////

  // 3h. How many possible values exist of type Option[Nothing]?
  val optNothing: Cardinality[Option[Nothing]] = new Cardinality[Option[Nothing]] {
    def cardinality: Card = Constant(1) + nothing.cardinality
  }

  // 3i. How many possible values exist of type (Boolean, Nothing)?
  val boolNothing: Cardinality[(Boolean, Nothing)] = new Cardinality[(Boolean, Nothing)] {
    def cardinality: Card = boolean.cardinality * nothing.cardinality
  }

  // 3j. How many possible implementation exist for `getCurrency`?
  def getCurrency: Cardinality[Country => Currency] = new Cardinality[Country => Currency] {
    def cardinality: Card = ???
  }

  // 3k. How many possible implementation exist for `getCurrencyString`? Is it more or less than `getCurrency`?
  def getCurrencyString: Cardinality[String => Option[String]] = new Cardinality[String => Option[String]] {
    def cardinality: Card = ???
  }

  // 3l. How many possible values exist of type A => B?
  def func[A, B](a: Cardinality[A], b: Cardinality[B]): Cardinality[A => B] =
    new Cardinality[A => B] {
      def cardinality: Card = ???
    }

  // 3m. Can you think of a function signature with only one implementation?
  // i.e. find A1, A2 such as |A1 => A2| = 1.

  // 3n. Can you provide an example of a function signature with no implementation?
  // i.e. find A1, A2 such as |A1 => A2| = 0.

  ////////////////////////
  // 4. Parametricity
  ////////////////////////

  // 4a. How many implementations exist for `id` (assume we are using functional subset)?
  def id[A](a: A): A = ???

  // 4b. How many implementations exist for `mapOption`?
  def mapOption[A, B](opt: Option[A])(f: A => B): Option[B] = ???

  // 4c. How many implementations exist for `mapOptionIntToBool`?
  def mapOptionIntToBool(opt: Option[Int])(f: Int => Boolean): Option[Boolean] = ???

  // 4d. How would you test `mapOption` to be sure there is no bug?

  // 4e. How many implementations exist for `mapList`? How would you test it?
  def mapList[A, B](xs: List[A])(f: A => B): List[B] = ???

  ////////////////////////
  // 5. Tests
  ////////////////////////

  // 5a. Given `getCurrency` signature, what is the VIC of of `getCurrency`
  // if we have one unit test, e.g. assert(getCurrency(France) == EUR)?
  // If we have two unit tests, e.g. assert(getCurrency(France) == EUR) and assert(getCurrency(Germany) = EUR)?
  def getCurrency(country: Country): Currency = ???

  sealed trait Country
  object Country {
    case object France        extends Country
    case object Germany       extends Country
    case object UnitedKingdom extends Country
  }

  sealed trait Currency
  object Currency {
    case object EUR extends Currency
    case object GBP extends Currency
  }

  // 5b. Given `sign` signature, what is the VIC of of `sign`
  // if we have one unit test, e.g. assert(sign(-2) == false)?
  // If we have two unit tests, e.g. assert(sign(-2) == false), assert(sign(0) == true) and assert(sign(5) == true) ?
  def sign(x: Int): Boolean = ???

  // 5c. Can you define the VIC formula for any function A => B with n different unit tests?

  // 5d. What is the VIC of `sign` if it has the following property based test:
  // forAll(x: Int => sign(x) == !sign(-x)).

  // 5e. Can you define the VIC formula for any function A => B with n different property based tests?

  ////////////////////////
  // 6. Type Algebra
  ////////////////////////

  // 6a. In basic algebra, a * 1 = 1 * a = a and a + 0 = 0 + a = a (we say that 1 is the unit of * and 0 is the unit of +).
  // Is it also true with types?
  // To prove that two types A and B are equivalent you need to provide a pair of functions `to` and `from`
  // such as for all a: A, from(to(a)) == a, and equivalent for B.
  def aUnitToA[A]: Iso[(A, Unit), A] =
    Iso[(A, Unit), A](
      { case (a, b) => ??? },
      a => ???
    )

  def aOrNothingToA[A]: Iso[Either[A, Nothing], A] =
    Iso(_ => ???, _ => ???)

  // 6b. Prove that `Option[A]` is equivalent to `Either[Unit, A]`.
  def optionToEitherUnit[A]: Iso[Option[A], Either[Unit, A]] =
    Iso(_ => ???, _ => ???)

  // 6c. Prove that a * (b + c) = a * b + a * c.
  def distributeTuple[A, B, C]: Iso[(A, Either[B, C]), Either[(A, B), (A, C)]] =
    Iso(_ => ???, _ => ???)

  // 6d. Prove that a ^ 1 = a.
  def power1[A]: Iso[Unit => A, A] =
    new Iso[Unit => A, A](
      _ => ???,
      _ => ???
    )

  // 6e. Can you think of any other properties that types and algebra have in common?

}
