package basics

import swiftvis2.plotting._
import swiftvis2.plotting.styles.ScatterStyle
import swiftvis2.plotting.renderer.SwingRenderer

case class TempRow(day: Int, doy: Int, month: Int, year: Int, precip: Double,
    tave: Double, tmax: Double, tmin: Double)

object SATemps
{
    def parseLine(line: String) : TempRow = 
    {
        val p = line.split(',')
        TempRow(p(0).toInt, p(1).toInt, p(2).toInt, p(4).toInt, p(5).toDouble,
            p(6).toDouble, p(7).toDouble, p(8).toDouble)
    }
//foldr -> array(doubles) -> max temperatures 
    def main(args: Array[String]): Unit = {
        val source = scala.io.Source.fromFile("/users/mlewis/CSCI3395-F19/InClassBD/data/SanAntonioTemps.csv")
        val lines = source.getLines()
        val data = lines.drop(2).map(parseLine).toArray
        data.take(5).foreach(println)
        
        val hotDay1 = data.maxBy(_.tmax)
        val hotDay2 = data.reduce((d1, d2) => if (d1.tmax > d2.tmax) d1 else d2)

        println(hotDay1)
        println(hotDay2)
    
        val precDay = data.maxBy(_.precip)
        println("precip" + precDay)

        val numRainDays = data.filter(p => p.precip > 1).length

        val frac = (numRainDays.toDouble / data.length.toDouble)

        print(frac)

        val rainyDays = data.filter(_.precip >= 1.0)
        val rainyTemp = rainyDays.foldLeft(0.0)(_ + _.tmax) / rainyDays.length
        val (rainySum, rainyCount) = data.foldLeft((0.0, 0)){case ((sum, count), day) =>
            if (day.precip >= 1) (sum + day.precip, count + 1) else (sum, count)
        }
       
        println(rainySum/rainyCount)
        val months = (data.groupBy(_.month))
        val avgHighByMonth = months.mapValues((rows) => rows.map(_.tmax).sum / rows.length)
        val avgPrecipByMonth = months.mapValues((rows) => rows.map(_.precip).sum / rows.length)
        val medByMonth = months.mapValues(month => month.sortBy(_.precip).apply(month.length/2))

        avgPrecipByMonth.toSeq.sortBy(_._1).foreach(println)
        medByMonth.toSeq.sortBy(_._1).foreach(println)

        val cg = ColorGradient(1946.0 -> RedARGB, 1975.0 -> BlueARGB, 2014.0 -> GreenARGB)
        val sizes = data.map(_.precip * 2 + 2)
        val tempByDayPlot = Plot.simple(ScatterStyle(data.map(_.doy), data.map(_.tave), symbolWidth = sizes, symbolHeight = sizes, colors = cg(data.map(_.year))),
         "Day of Year", "Temp", "SA Temps") 
        SwingRenderer(tempByDayPlot, 800, 800, true)
    }

}