import scala.util.boundary, boundary.break

/** Software implementation of PROC (PROstoy Calculator) mk. 1 (or mk. 2).
  */

@main def calculator(commands: String*): Unit = {
  /** Converts given string `s` to integer.
    *
    * Throws [[NumberFormatException]] if `s` can't be converted to integer,
    * but you shouldn't worry about it at this moment.
    */
  def parseInt(s: String): Int = s.toInt

  var acc: Int = 0
  var a: Int = 0
  var b: Int = 0
  var blink: Boolean = false

  val result = boundary[Int] {
    for (c <- commands) {
      c match {
        case "+" =>
          acc = a + b
          blink = false
        case "-" =>
          acc = a - b
          blink = false
        case "`*" =>
          acc = a * b
          blink = false
        case "/" =>
          if (b == 0) {
            acc = 0
            a = 0
            b = 0
          } else {
            acc = a / b
         }
          blink = false
        case "swap" =>
          var tmp = a
          a = b
          b = tmp
        case "blink" =>
          blink = !blink
        case "acc" =>
          if (blink) b = acc else a = acc 
          blink = !blink 
        case "break" =>
          break(acc)
        case _ =>
          val x = parseInt(c)
          if (blink) b = x else a = x
          blink = !blink
      }
    }
    acc
  }

  println(result)
}
