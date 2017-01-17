package com.gray.logic.mixin

import scala.reflect.{ClassTag, classTag}
import scala.util.{Try, Success, Failure}


trait ControlFlow {

  def continueIf[T](condition: Boolean)(block: Option[T]) = if (condition) block else None

  def continueAs[T:ClassTag, U](thing: Any)(f: (T) => Option[U] ): Option[U] = thing match {
    case t if classTag[T].runtimeClass.isInstance(t) => f(t.asInstanceOf[T])
    case _ => None
  }

  def continueWithUnitAs[T: ClassTag](thing: Any)(f: (T) => Unit) : Unit = thing match {
    case t if classTag[T].runtimeClass.isInstance(t) => f(t.asInstanceOf[T])
    case _ => Unit
  }

  def continueWith[T,U](thing: T)(partialFunction: PartialFunction[T,Option[U]]) = if (partialFunction.isDefinedAt(thing)) {
    partialFunction(thing)
  }else None


}
