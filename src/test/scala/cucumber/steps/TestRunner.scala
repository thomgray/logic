package cucumber.steps

import cucumber.api.CucumberOptions
import cucumber.api.junit.Cucumber
import org.junit.runner.RunWith

@RunWith(classOf[Cucumber])
@CucumberOptions(
//  features = Array("classpath:cucumber/"),
//  glue = Array("src/test/scala/cucumber/steps"),
//  tags = Array("@wip"),
  monochrome = false,
  plugin = Array("pretty",
    "html:target/cucumber",
    "json:target/cucumber/test-report.json",
    "junit:target/cucumber/test-report.xml")
)
class TestRunner {}