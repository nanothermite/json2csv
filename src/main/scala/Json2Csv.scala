import java.io.{BufferedWriter, File, FileWriter}

import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

/**
 * Created by hkatz on 4/30/15.
 * CopyRight (C) ISCS, Inc 2015 All Rights Reserved
 *
 * Task #1 - Convert any JSON data to CSV format

The JSON data is not a uniform type. The converted CSV must:

    contain a column name header row
    be a single file and not multiples
    variable columns based on the input data
    column names must be unique
    not combine multiple data elements into one

The naming convention of the columns is not important but must be
mappable to the original JSON.
Run your implementation on the data sample given but it must work on any valid JSON.

 Task #2

Create a program that would be given to users to extract the following from
a local data file that is in the same format as the sample.

1) Total Distinct Types of activity and counts.
2) Total Weight of all sending packages.
 */
object Json2Csv {
  var header = ArrayBuffer[String]()
  var hdr_map = scala.collection.mutable.HashMap[String, Int]()
  var reduced_item = scala.collection.mutable.HashMap[String, Any]()

  /**
   * build map of col name -> col value and grow col header name
   * @param key header builder
   * @param value node element
   */
  def reduce_item(key: String, value: Any) : Unit = {
    value match {
      case List(xs @ _ *) =>
        xs.zipWithIndex.foreach {
          case (item, index) => reduce_item(s"${key}_$index", item)
        }

      case m: scala.collection.immutable.Map[_, _] =>
        for ((k:String, v) <- m) {
          val newKey = if (key != "") s"${key}_$k" else s"$k"
          reduce_item(newKey, v)
        }

      case _  => if (!hdr_map.contains(key)) {
        hdr_map += (key -> 1)
      } else {
        hdr_map += (key -> (hdr_map(key) + 1))
      }
      val newkey = s"${key}_${hdr_map(key)}"
      reduced_item += (newkey -> value); header += newkey
    }
  }

  val hdr_type = "header_type"
  val hdr_weight = "weight"

  def main(args: Array[String]) = {
    val tracking = if (args.length == 3) true else false
    if (args.length != 2 && !tracking) {
      sys.error("\nUsage: scala json2csv.scala <json_in_file_path> <csv_out_file_path>\n")
      sys.exit()
    }
    //inputs
    val jsonObj = parse(new File(args(0)))
    // outputs
    val csvFile = new File(args(1))
    val bw = new BufferedWriter(new FileWriter(csvFile))
    //parsing
    val rootNode = ""
    var typeCounts = scala.collection.mutable.HashMap[String, BigInt]()
    var totWt:Double = 0.0
    for (child <- jsonObj.children) {
      reduce_item(rootNode, child.values)

      if (tracking) {   // reporting section
        val hdr_str:String = reduced_item(hdr_type).asInstanceOf[String]
        if (typeCounts.contains(hdr_str)) {
          typeCounts(hdr_str) = typeCounts(hdr_str) + 1
        } else {
          typeCounts += hdr_str -> 1
        }
        val wt_keys = reduced_item.keySet.filter(_.endsWith(hdr_weight))
        if (wt_keys.nonEmpty)
          totWt = reduced_item.filterKeys{ wt_keys.contains(_) == true }.map(_._2.asInstanceOf[Double]).sum
      }
    }
    val hdr = s"${header.mkString(",")}"
    val vals = s"${header.map(x => reduced_item(x)).mkString(",")}"

    println(hdr)
    println(vals)

    bw.write(s"$hdr\n")
    bw.write(s"$vals\n")

      //header.clear()
      //reduced_item.clear()

    bw.close()

    if (tracking)
      println(
        s"""
          |Data Types:
          |  call: ${typeCounts("call")}
          |  delivery: ${typeCounts("delivery")}
          |  sending: ${typeCounts("sending")}
          |Total Weight: $totWt
        """.stripMargin)
  }

}
