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
    val p2 = Location(-1.1, 79.66)
    val d = Visualization.greatCircleMetric(p1, p2)
    assert(d === math.Pi)
  }
  test("greatCircleMetric antipodes 2") {
    val p1 = Location(0.0, 10.8)
    val p2 = Location(0.0, -169.2)
    val d = Visualization.greatCircleMetric(p1, p2)
    assert(d === math.Pi)
  }
  test("greatCircleMetric of two points is positive") {
    val p1 = Location(10.1, -100.34)
    val p2 = Location(-1.7, 100.34)
    assert(Visualization.greatCircleMetric(p1,p2) > 0)
  }
  
  test("inverse distance weighting of two equal points") {
    val p1 = Location(18.01, -10.88)
    val p2 = Location(18.01, -10.88)
    assert(Visualization.idw(p1)(p2) === Double.PositiveInfinity)
    /*
    an [ArithmeticException] should be thrownBy {
      Visualization.idw(p1)(p2)
    } */
  }

  test("interpolateColor finds equal temperature") {
    val points = Seq( (10.0, Color(255, 0, 0)), (45.2, Color(100, 100, 0)) )
    val value = 10.0
    assert(Visualization.interpolateColor(points, value) === Color(255, 0, 0))
  }
  
  test("interpolateColor when temperature value higher than the points") {
    val points = Seq( (10.0, Color(255, 0, 0)), (45.2, Color(100, 100, 0)) )
    val value = 50.0
    assert(Visualization.interpolateColor(points, value) === Color(100, 100, 0))
  }

  test("interpolateColor when temperature value lower than the points") {
    val points = Seq( (0.5, Color(10, 25, 150)),
      (10.0, Color(255, 0, 0)), (45.2, Color(100, 100, 0)) )
    val value = -5.5
    assert(Visualization.interpolateColor(points, value) === Color(10, 25, 150))
  }

  test("interpolateColor between equally distanced points") {
    val points = Seq( (10.0, Color(100, 0, 50)), (30.0, Color(120, 100, 10)) )
    val value = 20.0
    assert(Visualization.interpolateColor(points, value)
      === Color(110, 50, 30))
  }
  
  test("interpolateColor when temperature difference doesn't divide color difference") {
    val points = Seq( (1.0, Color(10, 0, 100)), (4.0, Color(5, 52, 0)) )
    val value = 2.0
    assert(Visualization.interpolateColor(points, value)
      === Color(8, 17, 67))
  }

  test("interpolateColor official test") {
    val points = List((-1.684301E7,Color(255,0,0)), (0.0,Color(0,0,255)))
    val value = -8421505.0
    assert(Visualization.interpolateColor(points, value) === Color(128,0,128))
  }
  
}
