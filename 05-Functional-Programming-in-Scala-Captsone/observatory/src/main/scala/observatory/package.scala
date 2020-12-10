package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1

  val EarthRadius: Double = 6371

  val TileWidth = 256
  val TileHeight = 256
  val TileAlpha = 127

  val TemperatureColors = Seq(
    (-60.0, Color(0, 0, 0)),
    (-50.0, Color(33, 0, 107)),
    (-27.0, Color(255, 0, 255)),
    (-15.0, Color(0, 0, 255)),
    (0.0, Color(0, 255, 255)),
    (12.0, Color(255, 255, 0)),
    (32.0, Color(255, 0, 0)),
    (60.0, Color(255, 255, 255))
  )

  val DeviationColors = Seq(
    (7.0, Color(255, 255, 255)),
    (4.0, Color(255, 0, 0)),
    (2.0, Color(255, 255, 0)),
    (0.0, Color(255, 255, 255)),
    (-2.0, Color(0, 255, 255)),
    (-7.0, Color(0, 0, 255))
  )

  def temperaturesFileName(year: Year, tile: Tile): String = s"target/temperatures/$year/${tile.zoom}/${tile.x}-${tile.y}.png"

  def deviationsFileName(year: Year, tile: Tile): String = s"target/deviations/$year/${tile.zoom}/${tile.x}-${tile.y}.png"

}
