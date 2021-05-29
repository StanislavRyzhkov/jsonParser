package company.ryzhkov

object Application extends App {
  val str = """{"foo":"asd\"sdf"}"""

  str.toCharArray foreach { ch =>
    println(s"${ch.toInt} = $ch")
  }
}
