package pl.maxmati.agds

import java.lang.reflect.Field

import pl.maxmati.agds.loader.{Iris, Loader}

import scala.collection.{SortedSet, mutable}
import scala.collection.mutable.Queue


/**
  * Created by maxmati on 5/26/18 
  */

abstract class AgdsConnected {
  var x = 0d

  def process(): List[AgdsConnected]
}

case class AgdsRow[A](row: A, var entries: List[AgdsEntry[A]]) extends
  AgdsConnected {
  override def process(): List[AgdsConnected] = {
    val toProcess = entries.map { entry =>
      entry.disconnect(this)
      entry.x += x * 1
      entry
    }
    entries = List()
    toProcess
  }

  entries.foreach(_.connect(this))
}

case class AgdsEntry[A](value: Double, column: AgdsColumn[A])
  extends AgdsConnected with Ordered[AgdsEntry[A]] {
  var rowCount: Int = 0

  def build(): Unit = rowCount = rows.size

  private var rows = Set[AgdsRow[A]]()
  var prev: Option[AgdsEntry[A]] = None
  var next: Option[AgdsEntry[A]] = None

  override def process(): List[AgdsConnected] = {
    val toProcess = (prev.map { entry =>
      entry.next = None
      val dif = Math.abs(value - entry.value)
      val weight = 1d - (dif / column.range())
      entry.x += x * weight
      entry
    }.iterator ++ next.map { entry =>
      entry.prev = None
      val dif = Math.abs(value - entry.value)
      val weight = 1d - (dif / column.range())
      entry.x += x * weight
      entry
    }.iterator).toList

    rows.foreach { row =>
      row.entries = row.entries diff List(this)
      row.x += x * 1d / 5d
    }

    prev = None
    next = None
    rows = Set()

    toProcess
  }

  override def compare(that: AgdsEntry[A]): Int = value.compare(that.value)

  def connect(row: AgdsRow[A]): Unit = rows += row

  def disconnect(row: AgdsRow[A]): Unit = rows -= row
}

class AgdsColumn[A]() {
  def find(value: Double): AgdsEntry[A] = entries.find(_.value == value).get

  //TODO: proper agds find

  private var entries = SortedSet[AgdsEntry[A]]()

  def add(value: Double) {
    entries += AgdsEntry[A](value, this)
  }

  def build(): Unit = {
    entries.sliding(2).foreach { pair =>
      pair.head.next = Some(pair.last)
      pair.last.prev = Some(pair.head)
    }
    entries.foreach(_.build())
  }

  def min(): Double = entries.head.value

  def max(): Double = entries.last.value

  def range(): Double = max() - min()

}

class Agds[A](rawRows: List[A])(implicit m: Manifest[A]) {
  def calculate(rawRow: A): Unit = {
    var toProcess = mutable.Queue[AgdsConnected]()

    val row = rows.filter(_.row == rawRow).head
    row.x = 1
    toProcess ++= row.process()

    while (toProcess.nonEmpty) {
      val item = toProcess.dequeue()
      toProcess ++= item.process()
    }
  }

  private def getField(field: Field, row: A): Double = {
    field.setAccessible(true) // TODO: use scala compile macro
    field.getDouble(row)
  }

  private val columns: Map[String, AgdsColumn[A]]
  = m.runtimeClass.getDeclaredFields
    .map(field => field.getName -> new AgdsColumn[A])
    .toMap

  rawRows.foreach { row =>
    row.getClass.getDeclaredFields.foreach { field =>
      val value = getField(field, row)
      val columnName = field.getName
      columns(columnName).add(value)
    }
  }

  private val rows = rawRows.map { row =>
    AgdsRow(row, row.getClass.getDeclaredFields.map { field =>
      val columnName = field.getName
      columns(columnName).find(getField(field, row))
    }.toList)
  }

  columns.foreach(_._2.build())
}

object Main extends App {
  val data = new Loader().loadIris()
  val agds = new Agds(data)
  agds.calculate(data.head)
  print(data.head)
}
