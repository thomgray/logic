package cucumber.util

import com.gray.logic.formula._

object FormulaDictionary {

  private val dictionary = Map[String, Formula](
    "A" -> Sentence(0),
    "B" -> Sentence(1),
    "C" -> Sentence(2),

    "(A&B)" -> Conjunction(Sentence(0), Sentence(1))
  )

  def apply(key: String): Formula = dictionary(key)

}