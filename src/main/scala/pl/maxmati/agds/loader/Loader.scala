package pl.maxmati.agds.loader

import java.io.File
import kantan.csv._
import kantan.csv.ops._
import kantan.csv.generic._

/**
  * Created by maxmati on 5/26/18 
  */


class Loader {
  implicit val irisDecoder: RowDecoder[Iris] = RowDecoder.ordered {
    (sepalLength: Double, sepalWidth: Double, petalLength: Double,
     petalWidth: Double, className: String) =>
      Iris(sepalLength, sepalWidth, petalLength, petalWidth,
        Iris.classNames(className))
  }

  def loadIris(): List[Iris] = {
    new File("IrisDataTrain.csv")
      .readCsv[List, Iris](rfc.withoutHeader.withCellSeparator(' '))
      .collect { case Right(r) => r }
  }
}

object Iris {
  val classNames = Map(
    "Iris-setosa" -> 1,
    "Iris-versicolor" -> 2,
    "Iris-virginica" -> 3)
}

case class Iris(sepalLength: Double, sepalWidth: Double, petalLength: Double,
                petalWidth: Double, classId: Int) { // TODO: get back to Option
}
