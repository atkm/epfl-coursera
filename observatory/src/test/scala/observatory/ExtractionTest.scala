package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import java.time.LocalDate

trait ExtractionTest extends FunSuite {

  /*
  test("temperatures with one entry"){
    val oneLine = Extraction.locateTemperatures(2004, "/stations.csv", "/2004-one-entry.csv")
    Extraction.spark.stop
    val rounded = oneLine.map {case (date, loc, temp) => (date, loc, scala.math.round(temp*100).toDouble/100)}
    assert(rounded === Seq( (LocalDate.of(2004, 1, 1), Location(70.933,-8.667), -5.83) ))
  }
  */
  
}
