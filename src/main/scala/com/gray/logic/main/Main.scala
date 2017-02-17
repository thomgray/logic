package com.gray.logic.main

import com.gray.logic.deduction.Deduction
import com.gray.logic.deduction.inference.InferenceHard
import com.gray.logic.formula._
import com.gray.logic.language.{FormulaReaderAlphabetic, FormulaWriterAlphabetic}
import com.gray.logic.mixin.ControlFlow

import scala.collection.mutable
import scala.tools.jline._
import scala.tools.jline.console.ConsoleReader

object Main extends App with ControlFlow with LogicDSL {

  val terminal = TerminalFactory.create()
  val console = new ConsoleReader()

  console.setPrompt("⊢ ")

  implicit val writer = CustomWriter
  implicit val reader = FormulaReaderAlphabetic

  val P = a sentence
  val Q = a sentence
  val S = a sentence

  val `(PvQ)` = P or Q
  val `~(PvQ)` = not(P or Q)
  val `(S->~(PvQ))` = S implies not(P or Q)



  val result = Given(P or Q, S implies not (P or Q)) prove not (S)

  println(result.write)

  var continue = true

  val formulas = mutable.ListBuffer[Formula]()

  while (continue) {
    console.readLine().trim match {
      case "" =>
      case "exit" | "quit" => continue = false
      case "restart" => formulas.clear
        println("removed all formulas. Starting anew\n")
      case "show" =>
        formulas.foreach(f => println(" - "+f.write))
        println()
      case s if s.startsWith("+") =>
        val trimmed = s.stripPrefix("+").trim.replaceAll(" +", "")
        Formula.read(trimmed) match {
          case Some(formula) =>
            formulas += formula
            println(s"new formula ${formula.write}\n")
          case _ => println(s"couldn't read that: $trimmed\n")
        }
      case s if s.startsWith("prove") =>
        val formulaString = s.stripPrefix("prove").trim.replaceAll(" +", "")
        Formula.read(formulaString) match {
          case Some(formula) =>
            val deduction = new Deduction(formula, formulas) with InferenceHard
            deduction.prove(formula) match {
              case Some(_) =>
                println(deduction.write+"\n")
              case _ => println(s"Failed to prove ${formula.write}")
            }
          case _ => println(s"would love to, but that was illegible: $formulaString")
        }
      case _ => println("What do you want from me?")
    }
  }


}


object CustomWriter extends FormulaWriterAlphabetic {

}