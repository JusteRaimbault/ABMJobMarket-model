package org.igp.abmjobmarket

import scala.util.Random

object Utils {

  /**
   *  ! does not take into account quotes
   *
   * @param file file
   * @param columns column names
   * @param sep separator
   * @return
   */
  def readCSV(file: String, columns: Seq[String], sep: String = ",", withId: Boolean = false): Seq[Seq[String]] = {
    val source = io.Source.fromFile(file)
    val lines = source.getLines()
    val header = lines.next().split(sep)
    val headerInds = columns.map(header.indexOf(_))
    //println(header.toSeq+" ; "+headerInds.toSeq)
    val res = lines.zipWithIndex.map{case (s,i) =>
      val vals = s.split(sep)
      //println(vals.toSeq)
      val row = headerInds.map(i => vals(i))
      if (withId) Seq(i.toString)++row else row
    }.toSeq
    source.close()
    res
  }

  implicit class ArrayDecorator(a: Array[Double]) {
    def dot(b: Array[Double]): Double = {
      if (a.length!=b.length) throw new IllegalArgumentException("Dot product with vectors of different dimensions : "+a.toSeq+" . "+b.toSeq)
      a.zip(b).map{case (aa,bb) => aa*bb}.sum
    }
    def norm: Double = math.sqrt(a.map(aa => aa*aa).sum)
  }

  def randomDrawProbas[A](a: IterableOnce[A], p: IterableOnce[Double])(implicit rng: Random): A = {
    val r = rng.nextDouble()
    var s = p.iterator.next(); var currentElem = a.iterator.next()
    while (r>s&&p.iterator.hasNext&&a.iterator.hasNext) {s = s + p.iterator.next(); currentElem = a.iterator.next()}
    currentElem
  }

}
