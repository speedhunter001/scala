package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  val P = 6.0

  def greatCircleDistance(location1: Location, location2: Location): Double = {
    if (location1 == location2) 0.0
    else {
      val lon1 = location1.lon
      val lat1 = location1.lat
      val lat2 = location2.lat
      val lon2 = location2.lon

      val deltaAlpha = abs(lon1 - lon2)

      val centralAngle = math.sin(toRadians(lat1)) * sin(toRadians(lat2)) +
        cos(toRadians(lat1)) * cos(toRadians(lat2)) * cos(toRadians(deltaAlpha))

      if (centralAngle < -1.0) {
        EarthRadius * Pi
      } else if (centralAngle > 1.0) {
        0.0
      } else {
        EarthRadius * math.acos(centralAngle)
      }
    }
  }
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val (temperatureWeights, weights) = temperatures.aggregate((0.0, 0.0))(
      {
        case ((tempWeights, weights), (otherLocation, temperature)) =>
          val distance = greatCircleDistance(otherLocation, location)
          if (distance < 1.0) {
            return temperature
          }
          val weight = 1.0 / pow(distance, P)
          (tempWeights + temperature * weight, weights + weight)
      },
      {
        case ((tempWeights1, weights1), (tempWeights2, weights2)) =>
          (tempWeights1 + tempWeights2, weights1 + weights2)
      }
    )
    temperatureWeights / weights
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val sortedPoints = points.toIndexedSeq.sortBy(_._1)

    //    if (value <= sortedPoints.head._1) {
    //      return sortedPoints.head._2
    //    } else if (value >= sortedPoints.last._1) {
    //      return sortedPoints.last._2
    //    }

    for (i <- 0 until sortedPoints.length - 1) {
      (sortedPoints(i), sortedPoints(i + 1)) match {
        case ((t1, Color(r1, g1, b1)), (t2, Color(r2, g2, b2))) => {
          if (value < t1)
            return Color(r1, g1, b1)
          else if (value < t2) {
            val coeff = (value - t1) / (t2 - t1)
            return Color(
              round((1 - coeff) * r1 + coeff * r2).toInt,
              round((1 - coeff) * g1 + coeff * g2).toInt,
              round((1 - coeff) * b1 + coeff * b2).toInt
            )
          }
        }
      }
    }

    sortedPoints.last._2
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val pixels =
      for {
        lat <- 90 until -90 by -1
        lon <- -180 until 180
      } yield {
        val predicted_temperature = predictTemperature(temperatures, Location(lat, lon))
        val color = interpolateColor(colors, predicted_temperature)
        Pixel(color.red, color.green, color.blue, 255)
      }

    Image(360, 180, pixels.toArray)

  }

}

