package com.gray.logic.mixin

trait ReadUtils {

  def unwrapBrackets(string: String) = if (string.startsWith("(") && string.endsWith(")")) string.substring(1, string.length - 1) else string

  def rangeOfBrackets(string: String, from: Int = 0) = {
    string.charAt(from) match {
      case '(' =>
        def recursion(lr: Int, i: Int): Int = {
          if (i >= string.length) -1
          else {
            val newLr = string.charAt(i) match {
              case '(' => lr + 1
              case ')' => lr - 1
              case _ => lr
            }

            if (newLr == 0) {
              i
            } else {
              recursion(newLr, i + 1)
            }
          }
        }

        val end = recursion(1, from + 1)
        end match {
          case -1 => None
          case _ => Some((from, end + 1))
        }
      case _ => None
    }
  }

}
