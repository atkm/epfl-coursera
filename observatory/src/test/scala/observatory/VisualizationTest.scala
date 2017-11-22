package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalatest.Matchers._

trait VisualizationTest extends FunSuite with Checkers {

  test("greatCircleMetric equal points") {
    val p = Location(1.1, -100.34)
    val d = Visualization.greatCircleMetric(p, p)
    assert(d === 0)
  }
  test("greatCircleMetric equal points 2") {
    val p = Location(0.0, 0.0)
    val d = Visualization.greatCircleMetric(p, p)
    assert(d === 0)
  }

  test("greatCircleMetric antipodes") {
    val p1 = Location(1.1, -100.34)
    val p2 = Location(-1.1, 100.34)
    val d = Visualization.greatCircleMetric(p1, p2)
    assert(d === math.Pi)
  }
  test("greatCircleMetric antipodes 2") {
    val p1 = Location(0.0, -10.8)
    val p2 = Location(0.0, 10.8)
    val d = Visualization.greatCircleMetric(p1, p2)
    assert(d === math.Pi)
  }
  test("greatCircleMetric of two points is positive") {
    val p1 = Location(10.1, -100.34)
    val p2 = Location(-1.7, 100.34)
    assert(Visualization.greatCircleMetric(p1,p2) > 0)
  }
  
  test("inverse distance weighting of two very close points") {
    val p1 = Location(18.01, -10.88)
    val p2 = Location(18.01, -10.88)
    an [ArithmeticException] should be thrownBy {
      Visualization.idw(p1)(p2)
    }
  }
  
}
