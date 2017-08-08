abstract class JSON

case class JSeq(elems: List[JSON]) extends JSON

case class JObj(bindings: Map[String, JSON]) extends JSON

case class JNum(num: Double) extends JSON

case class JStr(str: String) extends JSON

case class JBool(b: Boolean) extends JSON

case object JNull extends JSON


val f: (String => String) = {
  case "ping" => "pong"
}

val g: PartialFunction[String, String] = {
  case "ping" => "pong"
}

f("ping")
g("ping")
g.isDefinedAt(("ping"))
g.isDefinedAt("abc")