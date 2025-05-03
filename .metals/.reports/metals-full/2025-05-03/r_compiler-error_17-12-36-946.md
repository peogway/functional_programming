file:///C:/Users/duchu/Fp_project/MyScalaProject/reps/src/main/scala/example/Main.scala
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 20980
uri: file:///C:/Users/duchu/Fp_project/MyScalaProject/reps/src/main/scala/example/Main.scala
text:
```scala
/* Group PGW
Hung Nguyen
Thanh Vinh Le
Hung Le */

import java.util.Date
import java.text.SimpleDateFormat
import scala.io.StdIn.readLine
import scala.io.Source
import scala.collection.immutable.ListMap
import scala.util.Random
import java.io.{File, PrintWriter}
import java.time.LocalDateTime
import java.time.LocalDate
import java.net.{HttpURLConnection, URL}
import java.nio.file.{Files, Paths}
import java.time.{ZonedDateTime, ZoneOffset}
import java.time.temporal.ChronoUnit
import scala.util.{Try, Success, Failure}
import io.circe._ // For JSON representation
import io.circe.parser._ // For parsing JSON
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

object Main {
  val energySources = "sources.csv"
  val API_KEY = Source.fromFile("apiKey.txt").getLines().mkString
  val lowHydroThreshold = 500.0
  val lowWindThreshold = 500.0
  val lowSolarThreshold = 50.0

  def alertTheshold(id: String): Double = {
    id match {
      case "191" => lowHydroThreshold
      case "181" => lowWindThreshold
      case "247" => lowSolarThreshold
      case "192" => 5000.0
      case _     => 0.0 // Default value for unknown sources
    }
  }
  val dataSources = Map(
    247 -> "solar.csv",
    191 -> "hydro.csv",
    181 -> "wind.csv",
    192 -> "data.csv"
  )

  // Define the sources
  case class SolarPanel(
      id: Int,
      capacity: Double,
      currentPower: Double,
      angle: Double
  )
  case class WindTurbine(
      id: Int,
      capacity: Double,
      currentPower: Double,
      direction: Double
  )
  case class HydroPlant(
      id: Int,
      capacity: Double,
      currentPower: Double,
      gateOpen: Boolean
  )

  // Control function (change settings)
  def adjustSolarAngle(s: SolarPanel, newAngle: Double): SolarPanel =
    s.copy(angle = newAngle)
  def rotateWindDirection(w: WindTurbine, newDir: Double): WindTurbine =
    w.copy(direction = newDir)
  def toggleHydroGate(h: HydroPlant): HydroPlant =
    h.copy(gateOpen = !h.gateOpen)

  // Utility function to safely convert a string to Double
  def safeToDouble(value: String): Double = {
    if (value.isEmpty) 0.0 // Return 0.0 if the string is empty
    else
      try {
        value.toDouble
      } catch {
        case _: NumberFormatException =>
          0.0 // Default to 0.0 on conversion failure
      }
  }

  def readSourcesFromFile(
      filename: String
  ): (List[SolarPanel], List[WindTurbine], List[HydroPlant]) = {
    // Open the file and read it
    val source = Source.fromFile(filename)

    // Lists to store the parsed data
    var solars: List[SolarPanel] = List()
    var winds: List[WindTurbine] = List()
    var hydros: List[HydroPlant] = List()

    // Skip header
    val lines = source.getLines().drop(1)

    // Process each line in the CSV file
    for (line <- lines) {
      val columns = line.split(",").map(_.trim)

      // Make sure to handle different cases based on the source type
      columns(0) match {
        case "Solar" =>
          val solar = SolarPanel(
            id = columns(1).toInt,
            capacity = safeToDouble(columns(5)),
            currentPower = safeToDouble(columns(6)),
            angle = safeToDouble(columns(2))
          )
          solars = solars :+ solar

        case "Wind" =>
          val wind = WindTurbine(
            id = columns(1).toInt,
            capacity = safeToDouble(columns(5)),
            currentPower = safeToDouble(columns(6)),
            direction = safeToDouble(columns(3))
          )
          winds = winds :+ wind

        case "Hydro" =>
          val hydro = HydroPlant(
            id = columns(1).toInt,
            capacity = safeToDouble(columns(5)),
            currentPower = safeToDouble(columns(6)),
            gateOpen = columns(4).toBoolean
          )
          hydros = hydros :+ hydro

        case _ => // Handle case when data source is invalid or not recognized
      }
    }

    source.close()

    // Return a tuple with lists
    (solars, winds, hydros)
  }
// Monitor function (display data)
  def monitorSource(source: Any): Unit = {
    source match {
      case s: SolarPanel =>
        println(
          s"Solar ${s.id}: Capacity = ${s.capacity} kW; Current Power = ${s.currentPower} kWh; Angle = ${s.angle}"
        )
      case w: WindTurbine =>
        println(
          s"Wind ${w.id}: Capacity = ${w.capacity} kW; Current Power = ${w.currentPower} kWh; Direction = ${w.direction}"
        )
      case h: HydroPlant =>
        println(
          s"Hydro ${h.id}: Capacity = ${h.capacity} kW, Current Power = ${h.currentPower} kW, Gate = ${if (h.gateOpen) "Opened" else "Closed"}"
        )
      case _ =>
        println("Unknown source type") // Default case for unknown types
    }
  }
  def monitorSources(): Unit = {
    val (solars, winds, hydros) = readSourcesFromFile("sources.csv")

    solars.foreach(monitorSource)
    println()
    winds.foreach(monitorSource)
    println()
    hydros.foreach(monitorSource)

    println("Do you want to adjust power's plant operation? (y/n)")
    val choice = readLine()
    if (choice == "y" || choice == "Y") {
      print(
        "Sources:\n1.\tSolar\n2.\tWind\n3.\tHydro\n4.\tBack\nEnter your choice: "
      )
      val option = readLine()
      option match {
        case "1" =>
          print("Enter solar panel ID: ")
          val solarId = readLine().toInt
          val solarPanel = solars.find(_.id == solarId)
          if (solarPanel.isEmpty) {
            println("Invalid solar panel ID")
            return
          }
          println(s"Current angle: ${solarPanel.get.angle}")
          print("Enter new angle: ")
          val newAngle = readLine().toDouble
          val updatedSolar = adjustSolarAngle(solarPanel.get, newAngle)

          // Replace the updated solar panel in the list
          val updatedSources = solars.map(s =>
            if (s.id == solarId) updatedSolar else s
          ) ++ winds ++ hydros

          // Save the updated list to the sources.csv file
          val writer = new PrintWriter(new File("sources.csv"))
          writer.println(
            "Source,ID,Angle,Direction,GateOpen,Capacity,Current Power"
          ) // Write header
          updatedSources.foreach {
            case s: SolarPanel =>
              writer.println(
                s"Solar,${s.id},${s.angle},,,${s.capacity},${s.currentPower}"
              )
            case w: WindTurbine =>
              writer.println(
                s"Wind,${w.id},,${w.direction},,${w.capacity},${w.currentPower}"
              )
            case h: HydroPlant =>
              writer.println(
                s"Hydro,${h.id},,,${h.gateOpen},${h.capacity},${h.currentPower}"
              )
          }
          writer.close()

          println("Solar panel updated and saved to sources.csv.")

        case "2" =>
          print("Enter wind turbine ID: ")
          val windId = readLine().toInt
          val windTurbine = winds.find(_.id == windId)
          if (windTurbine.isEmpty) {
            println("Invalid wind turbine ID")
            return
          }
          println(s"Current direction: ${windTurbine.get.direction}")
          print("Enter new direction: ")
          val newDirection = readLine().toDouble
          val updatedWind = rotateWindDirection(windTurbine.get, newDirection)

          // Replace the updated wind turbine in the list
          val updatedSources = solars ++ winds.map(w =>
            if (w.id == windId) updatedWind else w
          ) ++ hydros

          // Save the updated list to the sources.csv file
          val writer = new PrintWriter(new File("sources.csv"))
          writer.println(
            "Source,ID,Angle,Direction,GateOpen,Capacity,Current Power"
          ) // Write header
          updatedSources.foreach {
            case s: SolarPanel =>
              writer.println(
                s"Solar,${s.id},${s.angle},,,${s.capacity},${s.currentPower}"
              )
            case w: WindTurbine =>
              writer.println(
                s"Wind,${w.id},,${w.direction},,${w.capacity},${w.currentPower}"
              )
            case h: HydroPlant =>
              writer.println(
                s"Hydro,${h.id},,,${h.gateOpen},${h.capacity},${h.currentPower}"
              )
          }
          writer.close()

          println("Wind turbine updated and saved to sources.csv.")

        case "3" =>
          print("Enter hydro plant ID: ")
          val hydroId = readLine().toInt
          val hydroPlant = hydros.find(_.id == hydroId)
          if (hydroPlant.isEmpty) {
            println("Invalid hydro plant ID")
            return
          }
          println(s"Current gate status: ${if (hydroPlant.get.gateOpen) "Open"
            else "Closed"}")
          print("Toggle gate status? (y/n): ")
          val toggleChoice = readLine()
          if (toggleChoice == "y" || toggleChoice == "Y") {
            val updatedHydro = toggleHydroGate(hydroPlant.get)

            // Replace the updated hydro plant in the list
            val updatedSources = solars ++ winds ++ hydros.map(h =>
              if (h.id == hydroId) updatedHydro else h
            )

            // Save the updated list to the sources.csv file
            val writer = new PrintWriter(new File("sources.csv"))
            writer.println(
              "Source,ID,Angle,Direction,GateOpen,Capacity,Current Power"
            ) // Write header
            updatedSources.foreach {
              case s: SolarPanel =>
                writer.println(
                  s"Solar,${s.id},${s.angle},,,${s.capacity},${s.currentPower}"
                )
              case w: WindTurbine =>
                writer.println(
                  s"Wind,${w.id},,${w.direction},,${w.capacity},${w.currentPower}"
                )
              case h: HydroPlant =>
                writer.println(
                  s"Hydro,${h.id},,,${h.gateOpen},${h.capacity},${h.currentPower}"
                )
            }
            writer.close()

            println("Hydro plant updated and saved to sources.csv.")
          } else {
            println("No changes made to hydro plant.")
          }

        case "4" =>
          return
        case _ =>
          println("Invalid choice")
          return
      }
    } else {
      return
    }
  }

  def collectData(): Unit = {

    // Helper function for rate-limiting
    def sleepWithExponentialBackoff(retries: Int): Unit = {
      val delay = Math.pow(2, retries).toInt
      println(s"Sleeping for $delay seconds before retrying...\n")
      Thread.sleep(delay * 1000) // sleep for delay in seconds
    }

    val now = java.time.ZonedDateTime
      .now(java.time.ZoneOffset.UTC)
      .truncatedTo(java.time.temporal.ChronoUnit.SECONDS)

    // Collect data from the API
    println("Collecting data...")

    // Iterate over the data sources, not following functional programming paradigm
    for ((k, v) <- dataSources) {
      val url = new URL(
        s"https://data.fingrid.fi/api/datasets/$k/data?startTime=${now.minusMonths(3).toString}&endTime=${now.toString}&format=csv&pageSize=20000"
      )
      var attempts = 0
      var success = false
      // Try to connect to the API endpoint
      while (attempts < 5 && !success) { // Retry up to 5 times
        Try(url.openConnection().asInstanceOf[HttpURLConnection]) match {
          case Success(connection) =>
            connection.setRequestMethod("GET")
            connection.setRequestProperty("Accept", "text/csv")
            connection.setRequestProperty("x-api-key", API_KEY)
            connection.connect()

            Try(
              scala.io.Source
                .fromInputStream(connection.getInputStream)
                .mkString
            ) match {
              case Success(response) =>
                // Parse and write to file
                parse(response) match {
                  case Right(json) =>
                    json.hcursor.downField("data").as[String] match {
                      case Right(dataValue) =>
                        Files.write(Paths.get(v), dataValue.getBytes("UTF-8"))
                        println(s"Written data to file: $v\n")
                        success = true
                      case Left(error) =>
                        println(s"Error extracting 'data' field: $error")
                    }
                  case Left(error) =>
                    println(s"Error parsing JSON response: $error")
                }
              case Failure(e) =>
                println(s"Error reading response: ${e.getMessage}")
            }
          case Failure(e) =>
            println(s"Error connecting to API: ${e.getMessage}")
        }

        if (!success) {
          attempts += 1
          if (attempts < 5) {
            sleepWithExponentialBackoff(attempts) // Wait before retry
          }
        }
      }
    }
    println("Collection completed")
  }

  // Compare timestamps
  def compareTime(
      firstTimeStamp: Date,
      data: List[String],
      filter: Int,
      format: SimpleDateFormat
  ): Map[Date, Double] = {
    // Helper function
    def compareTimeHelper(
        data: List[String],
        interval: Date,
        acc: Map[Date, Double]
    ): Map[Date, Double] = {
      if (data.isEmpty) {
        acc
      } else {
        val cols = data.head.split(";").map(_.trim)
        val timestamp = cols(2)
        val date = format.parse(timestamp)
        if (date.compareTo(interval) >= 0) {
          compareTimeHelper(
            data.tail,
            interval,
            acc + (date -> cols(3).toDouble)
          )
        } else {
          compareTimeHelper(data.tail, interval, acc)
        }
      }
    }

    // Calculate the interval for last hour, last 24-hour, last week, last month or all time
    val interval = filter match {
      case 1 => new Date(firstTimeStamp.getTime - 60L * 60 * 1000)
      case 2 => new Date(firstTimeStamp.getTime - 24L * 60 * 60 * 1000)
      case 3 => new Date(firstTimeStamp.getTime - 7L * 24 * 60 * 60 * 1000)
      case 4 => new Date(firstTimeStamp.getTime - 30L * 24 * 60 * 60 * 1000)
      case 5 => new Date(0)
    }
    compareTimeHelper(data.drop(1), interval, Map.empty)
  }

  // Filter data based on user input
  def filterData(
      data: List[String],
      compareFn: (
          Date,
          List[String],
          Int,
          SimpleDateFormat
      ) => Map[Date, Double]
  ): Map[Date, Double] = {
    if (data == null) {
      return null
    }
    // Get last timestamp
    val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")

    val firstTimeStamp = format.parse(data(1).split(";")(2).trim)
    print(
      "Filter by:\n1.\tLast hour\n2.\tLast 24 hours\n3.\tLast week\n4.\tLast month\n5.\tAll\n6.\t Return Menu\nEnter choice: "
    )
    val choice = readLine()
    println()
    // Filter data based on user choice
    val filteredTimestamps = choice match {
      case "1" =>
        compareFn(firstTimeStamp, data, 1, format)
      case "2" =>
        compareFn(firstTimeStamp, data, 2, format)
      case "3" =>
        compareFn(firstTimeStamp, data, 3, format)
      case "4" =>
        compareFn(firstTimeStamp, data, 4, format)
      case "5" =>
        compareFn(firstTimeStamp, data, 5, format)
      case "6" =>
        return Map.empty[Date, Double]
      case _ =>
        println("Invalid choice")
        Map.empty[Date, Double]
    }
    filteredTimestamps
  }

  def readData(): List[String] = {
    print(
      "Sources:\n1.\tSolar\n2.\tWind\n3.\tHydro\n4.\tAll\n5.\tBack\nEnter your choice: "
    )
    val choice = readLine()
    println()
    // Read data from the selected source
    try {
      val bufferedSource = choice match {
        case "1" =>
          Source.fromFile("solar.csv")
        case "2" =>
          Source.fromFile("wind.csv")
        case "3" =>
          Source.fromFile("hydro.csv")
        case "4" =>
          Source.fromFile("data.csv")
        case "5" =>
          return null
        case _ =>
          println("Invalid choice")
          null
      }
      val data = bufferedSource.getLines.toList
      // def printDataHelper(data: List[String]): Unit = {
      //   if (data.nonEmpty){
      //     val cols = data.head.split(",").map(_.trim)
      //     println(
      //       s"${cols(0)}: \t${cols(1)}\t${cols(2)}\t${cols(3)}"
      //     )
      //     printDataHelper(data.tail)
      //   } else {
      //     println("No data available")
      //   }
      // }
      // printDataHelper(data)
      bufferedSource.close
      data
    } catch {
      case e: Exception =>
        null
    }
  }

  // Sort the data by timestamp or value
  def sortData(data: Map[Date, Double]): Map[Date, Double] = {
    print(
      "Sort by:\n1.\tTimestamp (Ascending)\n2.\tTimestamp (Descending)\n3.\tValue (Ascending)\n4.\tValue (Descending)\n5.\tReturn menu\nEnter choice: "
    )
    if (data == null) {
      return null
    }
    val choice = readLine()
    println()
    // Sort data based on user choice
    val sortedData = choice match {
      case "1" =>
        // Sort data by ascending timestamp
        println("Sorting data by ascending timestamp...")
        ListMap(data.toSeq.sortBy(_._1): _*)
      case "2" =>
        // Sort data by descending timestamp
        println("Sorting data by descending timestamp...")
        ListMap(data.toSeq.sortBy(_._1).reverse: _*)
      case "3" =>
        // Sort data by ascending value
        println("Sorting data by ascending value...")
        ListMap(data.toSeq.sortBy(_._2): _*)
      case "4" =>
        // Sort data by descending value
        println("Sorting data by descending value...")
        ListMap(data.toSeq.sortBy(_._2).reverse: _*)
      case "5" =>
        return Map.empty[Date, Double]
      case _ =>
        println("Invalid choice")
        Map.empty[Date, Double]
    }
    sortedData
  }

  def printData(data: Map[Date, Double]): Unit = {
    def printDataHelper(data: Map[Date, Double]): Unit = {
      if (data.nonEmpty) {
        val (k, v) = data.head
        println(s"${k}: \t${v}MW")
        printDataHelper(data.tail)
      }
    }

    if (data == null) {
      println("No data available")
    } else {
      println("\tTimestamp\t\tValue")
      printDataHelper(data)
    }
  }

  def viewEnergyGeneration(): Unit = {
    println("Viewing energy generation...")
    printData(sortData(filterData(readData(), compareTime)))

  }
  // Calculate the mean, median, mode, range and midrange of the data
  def analyzeSource(data: Map[Date, Double]): Unit = {
    println("Analyzing data...")
    if (data == null) {
      println("No data available")
      return
    }

    // Convert the data to a list
    val convertedData = data.values.toList

    // Calculate the mean
    val mean = convertedData.sum / convertedData.length
    println(f"Mean: $mean%.2f")

    // Calculate the median
    val median = {
      val (lower, upper) =
        convertedData.sortWith(_ < _).splitAt(convertedData.length / 2)
      if (convertedData.length % 2 == 0) (lower.last + upper.head) / 2.0
      else upper.head
    }
    println(s"Median: $median")

    // Calculate the mode
    val mode = convertedData
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .maxBy(_._2)
      ._1
    println(s"Mode: $mode")

    // Calculate the range
    val range = convertedData.max - convertedData.min
    println(f"Range: $range%.2f")

    // Calculate the midrange
    val midrange = (convertedData.max + convertedData.min) / 2
    println(f"Midrange: $midrange%.2f")
  }

  def analyzeData(): Unit = {
    analyzeSource(filterData(readData(), compareTime))
  }
  def searchData(): Unit = {
    printData(sortData(search(readData())))
  }

  def search(data: List[String]): Map[Date, Double] = {
    if (data == null) {
      return null
    }
    val format = new SimpleDateFormat("yyyy-MM-dd")
    print("Enter the date (yyyy/MM/dd) to search for: ")
    val date = readLine()
    Try(format.parse(date)) match {
      case Failure(_) =>
        println("Invalid date format. Please use yyyy/MM/dd.")
        return Map.empty[Date, Double]
      case Success(_) =>
        val searchDate = format.parse(date).date(@@)

        // Iterate through the data and add the data to the map if the date matches
        val newData =
          data.drop(1).foldLeft(Map.empty[Date, Double]) { (acc, line) =>
            val cols = line.split(";").map(_.trim)
            val timestamp = cols(2)
            val value = cols(3).toDouble
            if (format.parse(timestamp) == searchDate) {
              acc + ((new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"))
                .parse(timestamp) -> value)
            } else {
              acc
            }
          }
        return newData
    }
  }
  // Alert user if energy production is below the threshold
  def alertFn(): Unit = {
    try {
      val sources = dataSources.values.toList.dropRight(1)
      sources.map { source =>
        val bufferedSource = Source.fromFile(source)
        val data = bufferedSource.getLines.toList
        bufferedSource.close
        val minimum = alertTheshold(data(1).split(";")(0).trim)
        val alert = data.drop(1).foldLeft(false) { (acc, line) =>
          val cols = line.split(";").map(_.trim)
          val value = cols(3).toDouble
          if (value < minimum) {
            true
          } else {
            acc
          }
        }
        if (alert) {
          println(
            s"ALERT: ${source} has production values below the threshold of ${minimum}MW. Choose 6 for details!"
          )
        }
      }
    } catch {
      case e: Exception =>
        return
    }
  }

  // Scan data for anomalies
  def viewLowEnergyGeneration(data: List[String]): Map[Date, Double] = {
    if (data == null) {
      return null
    }
    val newData =
      data.drop(1).foldLeft(Map.empty[Date, Double]) { (acc, line) =>
        val cols = line.split(";").map(_.trim)
        val timestamp = cols(2)
        val value = cols(3).toDouble
        val minimum = alertTheshold(cols(0).trim)
        if (
          value < minimum && (cols(0).trim != "247" || cols(
            0
          ).trim == "247" && value > 0)
        ) {
          acc + ((new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"))
            .parse(timestamp) -> value)
        } else {
          acc
        }
      }
    newData
  }

  def operation6(): Unit = {
    printData(sortData(viewLowEnergyGeneration(readData())))
  }

  def main(args: Array[String]): Unit = {
    // Example usage - write data for the past 3 months to 'sources.csv'
    // Usage example

// Example: Print the Solar Panels data

    while (true) {
      println()
      alertFn()
      println()
      println(
        "REPS SYSTEM:\n1.\tMonitor sources\n2.\tCollect data\n3.\tView energy generation\n4.\tAnalyze data\n5.\tSearch data\n6.\tView low energy outputs\n0.\tExit\nPlease enter your choice: "
      )
      val choice = readLine()
      println()
      println()

      choice match {
        case "1" => monitorSources()
        case "2" => collectData()
        case "3" => viewEnergyGeneration()
        case "4" => analyzeData()
        case "5" => searchData()
        case "6" => operation6()
        case "0" => println("Exiting..."); System.exit(0)
        case _   => println("Invalid choice. Please try again.")
      }

      println()
    }
  }
}

//   }
// }

```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:129)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:244)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:101)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:46)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: -1