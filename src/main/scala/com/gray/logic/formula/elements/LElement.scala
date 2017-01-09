package com.gray.logic.formula.elements

abstract class LElement extends LElementConstants

abstract class LAtomicFormula extends LElement with LRootElement



case class LSentence(index: Int) extends LAtomicFormula

trait LRootElement