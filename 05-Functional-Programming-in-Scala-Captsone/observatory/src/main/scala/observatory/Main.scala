package observatory

import java.nio.file.{Files, Paths}

object Main extends App {

  def generateImage(year: Year, tile: Tile, data: Iterable[(Location, Temperature)]): Unit = {
    val path = Paths.get(s"target/temperatures/$year/${tile.zoom}/")
    Files.createDirectories(path)
    val image = Interaction.tile(data, TemperatureColors, tile)
    println(s"Create tile($tile)")
    image.output(new java.io.File(temperaturesFileName(year, tile)))
  }

  val year = 1975
  val filename = s"/$year.csv"

  val data = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(year, "/stations.csv", filename))

  println("Data extracted")

  Interaction.generateTiles(List((year, data)), generateImage)

}
