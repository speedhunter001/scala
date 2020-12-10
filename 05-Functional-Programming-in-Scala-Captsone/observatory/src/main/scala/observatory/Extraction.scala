package observatory

import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {
   def toCelsius(temp: Temperature): Temperature = (temp - 32) * 5 / 9

   def readData(path: String): Iterator[String] = {
     val stream = getClass.getResourceAsStream(path)
     Source.fromInputStream(stream, "utf-8").getLines()
   }
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationsRaw = readData(stationsFile)

    val stations = stationsRaw.map(_.split(",", -1))  // -1 is n param from java no matter how many times apply
                               .filter(row => (row(0).nonEmpty || row(1).nonEmpty) && row(2).nonEmpty && row(3).nonEmpty)
                               .map(row => (row(0), row(1)) -> Location(row(2).toDouble, row(3).toDouble))
                                .toMap

    val temperatureRaw = readData(temperaturesFile)

    val dataToReturn = temperatureRaw.map(_.split(",", -1))
                                     .filter(row => (row(0).nonEmpty || row(1).nonEmpty) && row(2).nonEmpty && row(3).nonEmpty && row(4).nonEmpty)
                                     .filter(row => stations.contains((row(0), row(1))))
                                     .map(row => (LocalDate.of(year, row(2).toInt, row(3).toInt), stations( (row(0), row(1)) ), toCelsius(row(4).toDouble)))
                                       .toSeq

    dataToReturn
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.map { case (_, location, temperature) => (location, temperature) }
      .groupBy { case (location, _) => location }
      .mapValues(loc => {
        val temperatures = loc.map(_._2)
        temperatures.sum / temperatures.size
      })
  }

}
