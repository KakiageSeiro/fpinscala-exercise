package EXERCISE4

// EXERCISE 4.6
sealed trait Either2[+E, +A] {
  def map[B](f: A => B): Either2[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either2[EE, B]): Either2[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def oeElse[EE >: E, B >: A](b: => Either2[EE, B]): Either2[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either2[EE, B])(f: (A, B) => C): Either2[EE, C] = this match {
    case Left(e) => Left(e)
    case Right(a) => b.map(bb => f(a, bb))
  }
}

case class Left[+E](value: E) extends Either2[E, Nothing]

case class Right[+A](value: A) extends Either2[Nothing, A]

//def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either2[Exception, Double] = {
//  for {
//    a <- Try(age.toInt)
//    tickets <- Try(numberOfSpeedingTickets.toInt)
//  } yield insuranceRateQuote(a, tickets)
//  // ↑ここでTryで包まれちゃうんだけどどうすればいいんだろう。
//}
//
//def insuranceRateQuote(i: Int, i1: Int): Either2[Exception, Double] = Right(1.0)