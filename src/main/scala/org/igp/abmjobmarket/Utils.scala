package org.igp.abmjobmarket

object Utils {

  /**
   *  ! does not take into account quotes
   *
   * @param file file
   * @param columns column names
   * @param sep separator
   * @return
   */
  def readCSV(file: String, columns: Seq[String], sep: String = ","): Seq[Seq[String]] = {
    val source = io.Source.fromFile(file)
    val lines = source.getLines()
    val header = lines.next().split(sep)
    val headerInds = columns.map(header.indexOf(_))
    val res = lines.map{s =>
      val vals = s.split(sep)
      val row = headerInds.map(i => vals(i))
      row
    }.toSeq
    source.close()
    res
  }

}
