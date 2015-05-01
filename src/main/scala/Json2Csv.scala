import java.io.{BufferedWriter, File, FileWriter}

import org.json4s._
import org.json4s.jackson.JsonMethods._

import scala.collection.immutable.Map
import scala.collection.mutable.ArrayBuffer

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
 */
object Json2Csv {
  var header = ArrayBuffer[String]()
  var reduced_item = scala.collection.mutable.LinkedHashMap[String, Any]()
  def reduce_item(key: String, value: Any) : Unit = {
    value match {
      case List(x, xs @ _ *) => {
        reduce_item(s"${key}_0", x)
        var i = 1
        for (item <- xs) {
          reduce_item(s"${key}_$i", item)
          i = i + 1
        }
      }

      case m: scala.collection.immutable.HashMap[_, _] => {
        for ((k:String, v) <- m) {
          val newKey = if (key != "") s"${key}_$k" else s"$k"
          v match {
            case s: String => { reduced_item += (newKey -> s); header += newKey }
            case i: BigInt => { reduced_item += (newKey -> i); header += newKey }
            case d: Double => { reduced_item += (newKey -> d); header += newKey }
            case List(head, tail) => reduce_item(newKey, v)
            case mi: Map[_,_] => reduce_item(newKey, mi)
          }
        }
      }

      case m: scala.collection.immutable.Map[_, _] => {
        for ((k:String, v:Any) <- m) {
          val newKey = if (key != "") s"${key}_$k" else s"$k"
          v match {
            case s: String => {reduced_item += (newKey -> s); header += newKey }
            case i: BigInt => { reduced_item += (newKey -> i); header += newKey }
            case d: Double => { reduced_item += (newKey -> d); header += newKey }
            case List(head, tail @ _ *) => reduce_item(newKey, v)
            case mi: Map[_,_] => reduce_item(newKey, mi)
          }
        }
      }

      case _ => {
        println(s"found $value")
      }
    }
  }

  def main(args: Array[String]) = {
    if (args.length != 2) {
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
    for (child <- jsonObj.children) {
      reduce_item(rootNode, child.values)

      val hdr = s"${header.mkString(",")}"
      val vals = s"${header.map(x => reduced_item.get(x).get).mkString(",")}"
      println(hdr)
      println(vals)

      bw.write(s"$hdr\n")
      bw.write(s"$vals\n")

      header.clear()
      reduced_item.clear()
    }

    bw.close()
  }

}
