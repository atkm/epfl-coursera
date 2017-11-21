package observatory

import java.time.LocalDate

import org.apache.log4j.{Level, Logger}

import org.apache.spark.sql._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._

/**
 * 1st milestone: data extraction
 */
object Extraction {
  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val spark = SparkSession
    .builder()
    .appName("Extraction")
    .config("spark.master", "local")
    .getOrCreate()
  
  import spark.implicits._

  /**
   * @param year             Year number
   * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
   * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
   * @return A sequence containing triplets (date, location, temperature)
   */

  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    locateTemperaturesDS(year, stationsFile, temperaturesFile)
  }

  // Spark Dataset solution
  def locateTemperaturesDS(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {

    val stationsRaw = spark.read.csv(getClass.getResource(stationsFile).getFile)
      .toDF(Seq("STN", "WBAN", "Lat", "Lon"): _*).na.drop(Seq("Lat", "Lon"))

    def formatCoords(lat: String, lon: String): Location = {
      def helper(coord: String): Double = {
        val sign = coord.head
        val value = coord.tail.toDouble
        sign match {
          case '+' => value
          case '-' => (-1)*value
          case _ => throw new Error("Something went wrong while formatting coordinates")
        }
      }
      
      Location(helper(lat), helper(lon))
    }
    
    val stations: Dataset[StationsRow] = stationsRaw.map {row =>
      StationsRow(row.getString(0), row.getString(1),
        formatCoords(row.getString(2), row.getString(3))
        )
    }
    
    // LocalDate doesn't have an encoder in Dataset.
    def formatLocalDate(month: String, day: String): LocalDate = 
      LocalDate.of(year, month.toInt, day.toInt)
    
    val temperaturesRaw = spark.read.csv(getClass.getResource(temperaturesFile).getFile)
    def toCelcius(fahrenheit: Temperature): Temperature = 5*(fahrenheit - 32)/9
    val temperatures: Dataset[TemperaturesRow] = temperaturesRaw.map { row =>
      TemperaturesRow(row.getString(0), row.getString(1), 
        year, row.getString(2).toInt, row.getString(3).toInt, toCelcius(row.getString(4).toDouble))
    }

    val joined = stations.join(temperatures,
      stations("stn")<=>temperatures("stn") && stations("wban")<=>temperatures("wban"))
        .drop(stations("stn")).drop(stations("wban"))
        .as[JoinedRow].collect

    joined.map {case JoinedRow(_, _, location, year, month, day, temp) =>
      (LocalDate.of(year, month, day), location, temp)
    }
  }


  /**
   * @param records A sequence containing triplets (date, location, temperature)
   * @return A sequence containing, for each location, the average temperature over the year.
   */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val tempsGrouped = records.groupBy(_._2).mapValues(_.map(_._3))
    tempsGrouped.mapValues(ls => ls.sum/ls.size)
  }

  def locationYearlyAverageRecordsDS(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {

    val rows: Iterable[RecordRow] = records.map {case (_, location, temp) =>
      RecordRow(location, temp)
    }
    val recordDS: Dataset[RecordRow] = rows.toList.toDS
    val averaged = recordDS.groupBy($"loc").avg("temp")
    val result = averaged.map {case Row(loc: Location, avgTemp: Double) =>
      (loc, avgTemp)
    }
    result.collect
  }

}

case class StationsRow( stn: String, wban: String, loc: Location )
case class TemperaturesRow( stn: String, wban: String,
  year: Int, month: Int, day: Int, temp: Temperature )
case class JoinedRow( stn: String, wban: String, loc: Location,
  year: Int, month: Int, day: Int, temp: Temperature )
case class RecordRow(loc: Location, temp: Temperature )

