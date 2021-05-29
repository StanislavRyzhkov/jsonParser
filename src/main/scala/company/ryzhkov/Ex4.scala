package company.ryzhkov

import java.io.{File, FileInputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

object Ex4 extends App {

  val res = classOf[Bar].getResource("")

  val path = s"${res}Bar.class"

  println(path)
  val fis = new File(path)

  println(fis.exists())

//  val xxx = Files.readAllBytes(Paths.get(path))
//
//  println(new String(xxx, StandardCharsets.UTF_8))

}
