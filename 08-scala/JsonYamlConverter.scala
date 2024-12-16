import scala.io.Source
import java.io.PrintWriter

object JsonYamlConverter {

  def countLeadingChar(s: String, char: Char): Int = {
    s.takeWhile(_ == char).length
  }

  def jsonToYaml(json: String): String = {
    // println("mid = ")
    // println(
    //   json
    //   .replace("{", "")
    //   .replace(",", "")
    //   .replace("\n}", "")
    //   .replace("\"", "")
    // )
    val yaml = json
      .replace("{", "")
      .replace(",", "")
      .replace("\n}", "")
      .replace("\"", "")
      .trim
    yaml
  }

  def yamlToJson(yaml: String): String = {
    var indent = 0
    val shift = 2
    var jsonString = "{\n"
    val lines = yaml.split("\n")
    for (line <- lines) {
      println(s"lline = $line")
      val splits = line.split(": ", 2)
      var new_indent = countLeadingChar(splits(0), ' ')
      if (new_indent < indent) {
        while (indent > new_indent) {
          indent -= shift
          jsonString += " " * indent + "},\n"
        }
      }
      if (new_indent > indent) {
        indent += shift
      }
      println(s"splits length = ${splits.length}")
      if (splits.length == 2) {
        jsonString += " " * indent + s"\"${splits(0).trim()}\": \"${splits(1).trim()}\",\n"
      } else {
        jsonString += " " * indent + s"\"${splits(0).trim().dropRight(1)}\": {\n"
      }
    }
    jsonString += "}\n"
    println(s"$jsonString")
    jsonString
  }

  def convertJsonToYaml(inputFile: String, outputFile: String): Unit = {
    val json = Source.fromFile(inputFile).getLines().mkString("\n")
    println("json = ")
    println(json)
    val yaml = jsonToYaml(json)
    // println("yaml = ")
    // println(yaml)

    val writer = new PrintWriter(outputFile)
    writer.write(yaml)
    writer.close()
  }

  def convertYamlToJson(inputFile: String, outputFile: String): Unit = {
    val yaml = Source.fromFile(inputFile).getLines().mkString("\n")
    // println(s"yaml = $yaml")
    val json = yamlToJson(yaml)
    println("received json")
    println(json)
    val writer = new PrintWriter(outputFile)
    writer.write(json)
    writer.close()
  }

  def main(args: Array[String]): Unit = {
    val jsonToYaml = false
    var conversion = ""
    var inputFile = ""
    var outputFile = ""
    if (jsonToYaml) {
      conversion = "jsonToYaml"
      inputFile = "input.json"
      outputFile = "output.yaml"
    } else {
      conversion = "yamlToJson"
      inputFile = "input.yaml"
      outputFile = "output.json"
    }
    println(jsonToYaml)
    println(inputFile)
    println(outputFile)

    conversion match {
      case "jsonToYaml" => convertJsonToYaml(inputFile, outputFile)
      case "yamlToJson" => convertYamlToJson(inputFile, outputFile)
      case _ => println("Invalid conversion type!")
    }
  }
}
