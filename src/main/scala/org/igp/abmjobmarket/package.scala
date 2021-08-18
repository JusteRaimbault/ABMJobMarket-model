package org.igp

/**
 *
 */
package object abmjobmarket {

  case class ModelParameters(

                            )


  case class ModelState(
                       workers: Seq[Worker],
                       employers: Seq[Employer],
                       jobs: Seq[Job]
                       )

  case class ModelResult()

}
