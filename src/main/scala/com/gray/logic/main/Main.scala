package com.gray.logic.main

import com.gray.logic.deduction.{Deduction, DeductionSequence}
import com.gray.logic.deduction.inference.InferenceSoft
import com.gray.logic.formula._
import com.gray.logic.language.{FormulaReaderAlphabetic, FormulaWriterAlphabetic, HumanReadable}
import com.gray.logic.mixin.ControlFlow

import scala.collection.mutable
import scala.tools.jline._
import scala.tools.jline.console.ConsoleReader

object Main extends App with ControlFlow with HumanReadable {

  val terminal = TerminalFactory.create()
  val console = new ConsoleReader()

  console.setPrompt("⊢ ")

  implicit val writer = CustomWriter
  implicit val reader = FormulaReaderAlphabetic

  val `(PvQ)` = Conjunction(Sentence(0), Sentence(1))
  val `~(PvQ)` = Negation(`(PvQ)`)
  val `(S->~(PvQ))` = Conditional(Sentence(2), `~(PvQ)`)

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
            val deduction = new Deduction(formula, formulas) with InferenceSoft
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