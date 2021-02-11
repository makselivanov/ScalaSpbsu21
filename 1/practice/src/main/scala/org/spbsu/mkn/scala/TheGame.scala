package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = {
    val sb = new StringBuilder
    var charSet = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).toSet
    for (i <- 1 to length) {
      val randIndex = Random.nextInt(charSet.size)
      val ch = charSet.drop(randIndex).head
      sb.append(ch)
      charSet -= ch
    }
    sb.toString()
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (secret.length != userInput.length)
      throw new WrongNumberLengthException(secret.length, userInput.length)
    if (userInput.toSet.size != userInput.length)
      throw new RepeatingDigitsException
    if (secret == userInput)
      return Correct(numTries)
    var cows = 0
    var bulls = 0

    for ((elem, index) <- userInput.zipWithIndex) {
      secret.indexOf(elem) match {
        case x if x == index =>
          bulls += 1
        case x if x >= 0 =>
          cows += 1
        case _ => ()
      }
    }
    Incorrect(bulls, cows)
  }

  def main(args: Array[String]): Unit = {
    print("Welcome to bulls and cows game! Please, enter length of string to guess: ")
    val length : Int = readLine().toInt
    val secret = generateNumberString(length)
    println("String generated, start guessing!")
    var numTries = 0
    var isGameEnd = false
    do {
      numTries += 1
      val string = readLine()
      val res = validate(secret, string, numTries)
      res match {
        case incorrect: Incorrect => println(s"Wrong! Bulls: ${incorrect.bulls} Cow: ${incorrect.cows}")
        case correct: Correct =>
          println(s"Correct! Number of tries: ${correct.numTries}")
          isGameEnd = true
      }
    } while (!isGameEnd)
  }
}
