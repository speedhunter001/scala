package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import observatory.Visualization._
import scala.math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  val zoomDepth = 8

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val x = tile.x
    val y = tile.y
    val n = pow(2, tile.zoom)

    Location(
      toDegrees( atan( sinh(Pi * (1.0 - 2.0 * y.toDouble / n)) ) ),
      x.toDouble / n * 360.0 - 180.0,
    )
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    imageFromTile(predictTemperature(temperatures, _), colors, tile)
  }

  def imageFromTile(predictTemperature: Location => Temperature, colors: Iterable[(Temperature, Color)],
                    tile: Tile): Image = {
    def toPixel(tile: Tile): Pixel = {
      val temperature = predictTemperature(tileLocation(tile))
      val color = interpolateColor(colors, temperature)
      Pixel(color.red, color.green, color.blue, TileAlpha)
    }

    val pixels = for {
      j <- (0 until TileHeight).par
      i <- 0 until TileWidth
    } yield toPixel(Tile(TileWidth * tile.x + i, TileHeight * tile.y + j, tile.zoom + zoomDepth))

    Image(TileWidth, TileHeight, pixels.toArray)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      (year, data) <- yearlyData
      zoom <- 0 to 3
      x <- 0 until pow(2, zoom).toInt
      y <- 0 until pow(2, zoom).toInt
    } generateImage(year, Tile(x, y, zoom), data)
  }

}
