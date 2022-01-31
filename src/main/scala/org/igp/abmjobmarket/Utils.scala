package org.igp.abmjobmarket

import java.util

import org.igp.abmjobmarket

import scala.jdk.CollectionConverters.IterableHasAsJava
import scala.reflect.ClassTag
import scala.util.Random

object Utils {

  def log(msg: String): Unit = {
    if (abmjobmarket.DEBUG) println(msg)
  }


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
    //println(columns)
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

  /**
   * Seems much more efficient with equiprobable objects
   * - weird since the while is bounded by O(|A|) and the cumsum is O(|A|) (and binarySearch is O(log(|A|)))
   * @param a objects
   * @param p probas
   * @param rng rng
   * @tparam A type
   * @return
   */
  def randomDrawProbas[A: ClassTag](a: IterableOnce[A], p: IterableOnce[Double])(implicit rng: Random): A = {

    val r = rng.nextDouble()

/*
    var s = p.iterator.next(); var currentElem = a.iterator.next();var i = 0;
    //println(s)
    while (r>s&&p.iterator.hasNext&&a.iterator.hasNext) {s = s + p.iterator.next(); currentElem = a.iterator.next();i=i+1}
    //println(s"random draw probas: $i ; $r > $s")
    currentElem
*/


    val cumsum: Array[Double] = p.iterator.toArray.map{var s = 0.0; d => {s += d; s}}
    val aarray = a.iterator.toArray[A]
    val insertionIndex = util.Arrays.binarySearch(cumsum, r)
    val ind = if (insertionIndex >= 0) insertionIndex else
      math.min(math.abs(insertionIndex + 1), aarray.length-1)
    aarray(ind)

  }


}
