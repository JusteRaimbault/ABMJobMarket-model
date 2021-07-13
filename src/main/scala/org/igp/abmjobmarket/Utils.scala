package org.igp.abmjobmarket

object Utils {

  def readCSV(file: String, columns: Seq[String], sep: String = ","): Seq[Seq[String]] = {
    val source = io.Source.fromFile(file)
    val lines = source.getLines()
    val header = lines.next().split(sep)
    val headerInds = columns.map(header.indexOf(_))
    val res = lines.map{s =>
      val vals = s.split(sep)
      headerInds.map(i => vals(i))
    }.toSeq
    source.close()
    res
  }

}
