package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    idwInterpolation(temperatures, location)
  }

  def idwInterpolation(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val nearest = temperatures.fold(temperatures.head) { case (x1, x2) =>
      if (greatCircleMetric(x1._1, location) <= greatCircleMetric(x2._1, location))
        x1 else x2
    }
    val distToNearest = greatCircleMetric(nearest._1, location) * 6371 // radius*sigma = distance
    if (distToNearest < 1) nearest._2
    else {
      val weights: Iterable[Double] = temperatures.map {case (loc, _) => idw(location)(loc)}
      val temps = temperatures.map(_._2)
      val product: Iterable[Double] = (temps zip weights).map {case (t,w) => t*w}
      product.sum/weights.sum
    }
  }

  def isZero(x: Double): Boolean =
    math.abs(x) <= math.pow(10,-4)
  
  def idw(x: Location)(y: Location): Double = {
    val d = greatCircleMetric(x,y)
    //if (d == 0.0) throw new ArithmeticException("The distance between two points is zero.")
    //else
    1/math.pow(d,2)
  }
  
  def degToRadian(x: Double): Double = math.Pi * x/180

  def greatCircleMetric(x: Location, y: Location): Double = {
    val Location(lat1, lon1) = x; val Location(lat2, lon2) = y
    (lat1, lon1, lat2, lon2) match {
      case _ if (x == y) => 0
      case _ if ((lat1 == (-1)*lat2)&&
        ((lon1 + 180 == lon2) || (lon1 - 180 == lon2))) => math.Pi
      case _ => {
        //val r = ??? // radius of the sphere doens't matter in weighting
        //val dlat = math.abs(lat1-lat2)
        val lon1Radian = degToRadian(lon1)
        val lon2Radian = degToRadian(lon2)
        val lat1Radian = degToRadian(lat1)
        val lat2Radian = degToRadian(lat2)
        val dlon = math.abs(lon1Radian - lon2Radian)
        val sigma = math.acos( math.sin(lat1Radian)*math.sin(lat2Radian)
          + math.cos(lon1Radian)*math.cos(lon2Radian)*math.cos(dlon) )
        sigma
      }
    }
  }


  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val opt = points.find(_._1 == value)
    opt match {
      case Some((_, col)) => col
      case None => {
        val (greater, lesser) = points.partition(_._1 > value)
        if (greater.isEmpty) lesser.maxBy(_._1)._2 
        else if (lesser.isEmpty) greater.minBy(_._1)._2 
        else {
          val upper = greater.minBy(_._1)
          val lower = lesser.maxBy(_._1)
          lower._2 + (upper._2 - lower._2) * ((value - lower._1) / (upper._1 - lower._1))
        }
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    // temperatures.roundLocation.groupBy(location).avg(temperature)
    val pixelColors: Iterable[(Temperature, Pixel)] =
      colors.map {case (temp, Color(r,g,b)) => (temp, Pixel(r,g,b,200))}
    // val img = Image(w: Int, h: Int, pixels: Array[Pixel])
    ???
  }

}

