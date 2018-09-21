/**
  * Created by LENOVO on 2016/12/9.
  */
object JsonObject extends App {

  abstract class JSON
  case class JSeq(elems: List[JSON]) extends JSON
  case class JObj(bindings: Map[String, JSON]) extends JSON
  case class JNum(num: Double) extends JSON
  case class JStr(str: String) extends JSON
  case class JBool(b:Boolean) extends JSON
  case object JNull extends JSON
  val data = JObj(Map(
    "firstName" -> JStr("John"),
    "address" -> JObj(Map(
      "streetAddress" -> JStr("21 2nd Street"),
      "state" -> JStr("NY"),
      "postalCode" -> JNum(10021)
    )),
    "phoneNumbers"->JSeq(List(
      JObj(Map(
        "type" -> JStr("home"),
        "number" -> JStr("101-110")
      )),
      JObj(Map(
        "type" -> JStr("fax"),
        "number" -> JStr("111-110")
      ))
    ))
  ))
  def showJson(json:JSON):String = json match{
    case JSeq(elems) =>
      "[" + (elems map showJson).mkString(",\n") + "]"
    case JObj(bindings) =>
      val assocs = bindings map {
        case (key, value) => "\"" + key + "\":" + showJson(value)
      }
      "{" + (assocs.mkString(",\n")) + "}"
    case JNum(num) => num.toString
    case JStr(str) => '\"' + str + '\"'
    case JBool(b) => b.toString
    case JNull => "null"
  }
  println(showJson(data))
  println(List(-1,1,0) withFilter (_>0))
  println(List(-1,1,0) filter (_>0))
  def findName(data: List[JSON]):List[JSON] = {
    for {
      JObj(bindings) <- data
      JSeq(phones) = bindings("phoneNumbers")
      JObj(phone) <- phones
      JStr(digits) = phone("number")
      if digits startsWith "101"
    } yield (bindings("firstName"))
  }
  println(showJson(List(data).head))
}
