package in.dogue.antiqua.utils

object FormatExc {
  def smallFormat(cols:Int, e:Exception) = {
    val lines: Array[StackTraceElement] = e.getStackTrace
    val buff = StringBuilder.newBuilder
    buff.append(e.getMessage + "\n")
    for (i <- 0 until math.min(30, lines.length)) {
      val line = lines(i)
      val name = line.getFileName.split('.')(0)
      val lineNum = line.getLineNumber
      buff.append("  %s:%d\n".format(name, lineNum))
    }
    buff.mkString
  }
}
