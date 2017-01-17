package cucumber.steps

import com.gray.logic.deduction.{Deduction, _}
import com.gray.logic.formula._

object DeductionHolder {

  def refresh() = {
    deduction = null
    conclusionNode = None
    conclusion = null
  }

  var deduction: Deduction = _
  var conclusionNode: Option[DeductionNode] = None
  var conclusion: Formula = _


}
