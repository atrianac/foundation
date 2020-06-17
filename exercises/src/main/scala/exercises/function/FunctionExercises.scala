package exercises.function

import scala.annotation.tailrec
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode
import scala.util.Random

// you can run and print things here
object FunctionApp extends App {
  import FunctionExercises._

  println("Hello World!")
}

object FunctionExercises {

  /////////////////////////////////////////////////////
  // 1. Functions as input (aka higher order functions)
  /////////////////////////////////////////////////////

  // Goals:
  // 1. Practice using standard higher order functions (e.g. map, filter)
  // 2. Get familiar with anonymous function syntax
  // 3. Start writing our own higher order functions

  // 1a. Implement `selectDigits` which iterates over a String and only keep the characters that are digits.
  // such as selectDigits("123foo0-!Bar~+3") == "12303"
  // but     selectDigits("hello") == ""
  // Note: You can use `filter` method from `String`, also check out the API of Char
  def selectDigits(text: String): String =
    text.filter(c => c.isDigit)

  // 1b. Implement `secret` which transforms all characters in a String to '*'
  // such as secret("Welcome123") == "**********"
  // Note: You can use `map` method from `String`
  def secret(text: String): String =
    text.map(_ => '*')

  // 1c. Implement `isValidUsernameCharacter` which checks if a character is suitable for a username.
  // We accept:
  // - lower and upper case letters
  // - digits
  // - special characters: '-' and '_'
  // For example, isValidUsernameCharacter('3') == true
  //              isValidUsernameCharacter('a') == true
  // but          isValidUsernameCharacter('^') == false
  // Note: You can remove the lazy keyword as soon you implement `isValidUsernameCharacter`.
  // It is only required to avoid tests throwing an exception.
  lazy val isValidUsernameCharacter: Char => Boolean =
    c => c.isLetterOrDigit || c == '-' || c == '_'

  // 1d. Implement `isValidUsername` which checks that all the characters in a String are valid
  // such as isValidUsername("john-doe") == true
  // but     isValidUsername("*john*") == false
  // Note: You can use `forAll` method from `String`
  def isValidUsername(text: String): Boolean =
    text.forall(c => isValidUsernameCharacter(c))

  case class Point(x: Int, y: Int) {
    // 1e. Implement `isPositive` which returns true if both `x` and `y` are greater or equal to 0, false otherwise
    // such as Point(2,  3).isPositive == true
    //         Point(0,  0).isPositive == true
    // but     Point(0, -2).isPositive == false
    // Note: `isPositive` is function define within `Point` class, so `isPositive` has access to `x` and `y`.
    // It is almost equivalent to define `isPositive` outside of `Point` with the following signature:
    // def isPositive(point: Point): Boolean
    def isPositive: Boolean =
      x >= 0 && y >= 0

    // 1f. Implement `isEven` which returns true if both `x` and `y` are even numbers, false otherwise
    // such as Point(2,  4).isEven == true
    // such as Point(0, -8).isEven == true
    // but     Point(3, -2).isEven == false
    def isEven: Boolean =
      x % 2 == 0 && y % 0 == 0

    // 1g. Both `isPositive` and `isEven` check that a predicate holds for both `x` and `y`.
    // Let's try to capture this pattern with a higher order function like `forAll`
    // such as Point(1,1).forAll(_ == 1) == true
    // but     Point(1,2).forAll(_ == 1) == false
    // Then, re-implement `isPositive` and `isEven` using `forAll`
    def forAll(predicate: Int => Boolean): Boolean =
      predicate(x) && predicate(y)
  }

  ////////////////////////////
  // 2. parametric functions
  ////////////////////////////

  case class Pair[A](first: A, second: A) {
    // 2a. Implement `swap` which exchanges `first` and `second`
    // such as Pair("John", "Doe").swap == Pair("Doe", "John")
    def swap: Pair[A] =
      Pair(second, first)

    // 2b. Implement `map` which applies a function to `first` and `second`
    // such as Pair("John", "Doe").map(_.length) == Pair(4,3)
    def map[B](f: A => B): Pair[B] =
      Pair(f(first), f(second))

    // 2c. Implement `forAll` which check if a predicate is true for both `first` and `second`
    // such as Pair(2, 6).forAll(_ > 0) == true
    // but     Pair(2, 6).forAll(_ > 2) == false
    //         Pair(2, 6).forAll(_ > 9) == false
    def forAll(predicate: A => Boolean): Boolean =
      predicate(first) && predicate(second)

    // 2d. Implement `zipWith` which merges two `Pair` using a `combine` function
    // such as Pair(0, 2).zipWith(Pair(3, 3), (x: Int, y: Int) => x + y) == Pair(3, 5)
    def zipWith[B, C](other: Pair[B], combine: (A, B) => C): Pair[C] =
      Pair(combine(first, other.first), combine(second, other.second))
  }

  val names: Pair[String] = Pair("John", "Elisabeth")
  val ages: Pair[Int]     = Pair(32, 46)
  case class User(name: String, age: Int)

  // 2f. Use Pair API to combine `names` and `ages` into `users`
  // such as `users` is equal to Pair(User("John", 32), User("Elisabeth", 46))
  lazy val users: Pair[User] = names.zipWith(ages, (name, age) => User(name, age))

  // 2g. Use Pair API to check the length of both String in `names` are strictly longer than 5
  lazy val longerThan5: Boolean = names.forAll(name => name.length >= 5)

  //////////////////////////////////////////////////
  // 3. functions as output (aka curried functions)
  //////////////////////////////////////////////////

  // 3a. Implement `increment` and `decrement` using `add`
  // such as increment(5) == 6
  // and     decrement(5) == 4
  // Note: You can remove the lazy keyword as soon you `increment` and `decrement`.
  def add(x: Int)(y: Int): Int = x + y

  lazy val increment: Int => Int = x => x + 1

  lazy val decrement: Int => Int = x => x - 1

  // 3b. Implement `formatDoubleCurried`, a curried version of `formatDouble`.
  def formatDouble(roundingMode: RoundingMode, digits: Int, number: Double): String =
    BigDecimal(number)
      .setScale(digits, roundingMode)
      .toDouble
      .toString

  lazy val formatDoubleCurried: RoundingMode => Int => Double => String =
    roundingMode => digits => number => BigDecimal(number)
      .setScale(digits, roundingMode)
      .toDouble
      .toString

  // 3c. Implement `format2Ceiling` using `formatDoubleCurried`
  // such as format2Ceiling(0.12345) == 0.13
  // Use RoundingMode.CEILING
  lazy val format2Ceiling: Double => String =
    number =>  formatDoubleCurried(RoundingMode.CEILING)(2)(number)

  /////////////////
  // 4. Iteration
  /////////////////

  // 4a. Implement `sum` using an imperative approach (for or while loop)
  // such as sum(List(1,5,2)) == 8
  // and     sum(List()) == 0
  def sum(xs: List[Int]): Int = {
    var sum = 0
    for(x <- 0 to xs.length)
      sum += xs(x)
    sum
  }

  // 4b. Implement `mkString` using an imperative approach (for or while loop)
  // such as mkString(List('H', 'e', 'l', 'l', 'o')) == "Hello"
  // and     mkString(List()) == ""
  def mkString(xs: List[Char]): String =
  {
    var sum = ""
    for(x <- 0 to xs.length)
      sum += xs(x)
    sum
  }

  // 4c. Implement `letterCount` using an imperative approach (for or while loop).
  // `letterCount` tells us how many times each letter appear in a `List`
  // such as letterCount(List('l', 'o', 'l')) == Map('l' -> 2, 'o' -> 1)
  // and     letterCount(List()) == Map()
  def letterCount(xs: List[Char]): Map[Char, Int] =
  {
    var letters = Map.empty[Char, Int]
    for(x <- 0 to xs.length)
      letters = addLetter(letters, xs(x))

    letters
  }

  def addLetter(state: Map[Char, Int], letter: Char): Map[Char, Int] =
    state.updatedWith(letter) {
      case None    => Some(1)
      case Some(n) => Some(n + 1)
    }

  // 4d. `sum`, `mkString`, `letterCount` are quite similar. Could you write a higher order function
  // that capture this pattern?
  // Hint: this method is called `foldLeft`.
  def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = {
    var acc = b
    for (a <- fa) acc = f(acc, a)
    acc
  }

  // 4e. Re-implement `sum` using `foldLeft`
  def sumFoldLeft(xs: List[Int]): Int =
    foldLeft(xs, 0)((b, a) => b + a)

  // 4f. Re-implement `mkString` using `foldLeft`
  def mkStringFoldLeft(xs: List[Char]): String =
    foldLeft(xs, "")((b, a) => b + a)

  // 4g. Re-implement `letterCount` using `foldLeft`
  def letterCountFoldLeft(xs: List[Char]): Map[Char, Int] =
    foldLeft(xs, Map.empty[Char, Int])((b, a) => addLetter(b, a))

  /////////////////
  // 5. Recursion
  /////////////////

  // 5a. `sumRecursive` and `letterCountRecursive` are versions of `sum` and `letterCount` using recursion.
  // These implementations have an issue, what is it?
  // Write a test to exhibit the problem and if you can, try to fix it (it is hard).
  def sumRecursive(xs: List[Int]): Int =
    xs match {
      case Nil          => 0
      case head :: tail => head + sumRecursive(tail)
    }

  def letterCountRecursive(xs: List[Char]): Map[Char, Int] =
    xs match {
      case Nil => Map.empty
      case head :: tail =>
        val letters      = letterCountRecursive(tail)
        val currentCount = letters.getOrElse(head, 0)
        letters.updated(head, currentCount + 1)
    }

  // 5b. Implement `foldLeftRecursive`, a recursive version of `foldLeft`.
  // If possible, try to make this implementation work on large list, e.g. List.fill(1000000)(1).
  def foldLeftRecursive[A, B](xs: List[A], b: B)(f: (B, A) => B): B =
    xs match {
      case head :: tail  => foldLeftRecursive(tail, f(b, head))(f)
      case Nil => b
    }

  ///////////////
  // 6. Laziness
  ///////////////

  // 6a. Implement `forAll` which checks if all elements in a List are true
  // such as forAll(List(true, true , true)) == true
  // but     forAll(List(true, false, true)) == false
  // Can you use foldLeft?
  // Does your implementation work with a large list? e.g. forAll(List.fill(1000000)(true)).
  // Does your implementation terminate early? e.g. forAll(List(false, false, false)) stops after seeing the first false.
  def forAll(xs: List[Boolean]): Boolean =
    xs match {
      case Nil        => true
      case false :: _ => false
      case true :: xs => forAll(xs)
    }

  // 6b. Implement `find` which returns the first element in a List where the predicate function returns true
  // such as find(List(1,3,10,2,6))(_ > 5) == Some(10)
  // but     find(List(1,2,3))(_ > 5) == None
  // Can you use foldLeft?
  // Does your implementation terminate early? e.g. find(List(1,2,3,4)(_ == 2) stop iterating as soon as it finds 2
  // Does your implementation work with a large list? e.g. find(1.to(1000000).toList)(_ == -1)
  def find[A](xs: List[A])(predicate: A => Boolean): Option[A] =
    xs match {
      case head :: _ if(predicate(head)) => Some(head)
      case _ :: tail => find(tail)(predicate)
      case Nil => None
    }

  def foldRight[A, B](xs: List[A], b: B)(f: (A, => B) => B): B =
    xs match {
      case Nil    => b
      case h :: t => f(h, foldRight(t, b)(f))
    }

  // 6c. Implement `forAllFoldRight` using `foldRight` (same behaviour than `forAll`)
  def forAllFoldRight(xs: List[Boolean]): Boolean =
    foldRight(xs, true)((a, b) => a && b)

  // 6d. Implement `findFoldRight` using `foldRight` (same behaviour than `find`)
  def findFoldRight[A](xs: List[A])(predicate: A => Boolean): Option[A] =
    foldRight(xs, Option.empty[A])((a, b) => if(predicate(a)) Some(a) else None )

  // 6e. Implement `headOption` using `foldRight`.
  // `headOption` returns the first element of a List if it exists
  // such as headOption(List(1,2,3)) == Some(1)
  // but     headOption(Nil) == None
  def headOption[A](xs: List[A]): Option[A] =
    foldRight(xs, Option.empty[A])((a, _) => Some(a))

  // 6f. Which fold would you use (left or right) to implement the following functions?
  def multiply(xs: List[Int]): Int =
    foldLeft(xs, 1)((b, a) => b * a)

  def min(xs: List[Int]): Option[Int] =
    xs match {
      case Nil          => None
      case head :: tail => Some(foldLeft(tail, head)(_ min _))
    }

  def filter[A](xs: List[A])(predicate: A => Boolean): List[A] =
    foldLeft(xs, List.empty[A])((b, a) => if(predicate(a)) b :+ a else b)

  ////////////////////////
  // 7. Pure functions
  ////////////////////////

  // 4a. is `plus` a pure function? why?
  def plus(a: Int, b: Int): Int = a + b

  // 4b. is `div` a pure function? why?
  def div(a: Int, b: Int): Int =
    if (b == 0) sys.error("Cannot divide by 0")
    else a / b

  // 4c. is `times2` a pure function? why?
  var counterTimes2 = 0
  def times2(i: Int): Int = {
    counterTimes2 += 1
    i * 2
  }

  // 4d. is `boolToInt` a pure function? why?
  def boolToInt(b: Boolean): Int =
    if (b) 5
    else Random.nextInt() / 2

  // 4e. is `mapLookup` a pure function? why?
  def mapLookup(map: Map[String, Int], key: String): Int =
    map(key)

  // 4f. is `times3` a pure function? why?
  def times3(i: Int): Int = {
    println("do something here") // could be a database access or http call
    i * 3
  }

  // 4g. is `circleArea` a pure function? why?
  val pi = 3.14
  def circleArea(radius: Double): Double =
    radius * radius * pi

  // 4h. is `inc` or inc_v2 a pure function? why?
  def inc_v2(xs: Array[Int]): Unit =
    for { i <- xs.indices } xs(i) = xs(i) + 1

  // 4i. is `incAll` a pure function? why?
  def incAll(value: Any): Any = value match {
    case x: Int    => x + 1
    case x: Long   => x + 1
    case x: Double => x + 1
  }

  // 4j. is `sumList` a pure function? why?
  def sumList(xs: List[Int]): Int = {
    var acc = 0
    xs.foreach(x => acc += x)
    acc
  }

  ////////////////////////
  // 5. Memoization
  ////////////////////////

  // 5a. Implement `memoize` such as
  // val cachedInc = memoize((_: Int) + 1)
  // cachedInc(3) // 4 calculated
  // cachedInc(3) // from cache
  // see https://medium.com/musings-on-functional-programming/scala-optimizing-expensive-functions-with-memoization-c05b781ae826
  // or https://github.com/scalaz/scalaz/blob/series/7.3.x/tests/src/test/scala/scalaz/MemoTest.scala
  def memoize[A, B](f: A => B): A => B = {
    val map = collection.mutable.HashMap.empty[A, B]

    (a: A) => map getOrElseUpdate (a, f(a))
  }

  // 5b. How would you adapt `memoize` to work on recursive function e.g. fibonacci
  // can you generalise the pattern?
  def memoize2 = ???

}
