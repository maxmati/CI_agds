package pl.maxmati.agds

import java.lang.reflect.Field

import pl.maxmati.agds.loader.Loader

import scala.collection.SortedSet
import scala.util.Random


/**
  * Created by maxmati on 5/26/18 
  */

abstract class AgdsConnected {
  var x = 0d
}

case class AgdsRow[A](row: A, var entries: List[AgdsEntry[A]]) extends
  AgdsConnected {
  def process(): Unit = {
    entries.foreach { entry =>
      entry.disconnect(this)
      entry.x += x * 1
      entry.process(forward = true, backward = true)
    }
    entries.foreach(_.connect(this))
  }

  entries.foreach(_.connect(this))

  override def toString: String = {
    s"$x: ${row.toString} – ${entries.size}"
  }
}

case class AgdsEntry[A](value: Double, column: AgdsColumn[A])
  extends AgdsConnected with Ordered[AgdsEntry[A]] {
  var rowCount: Int = 0

  def build(): Unit = rowCount = rows.size

  private var rows = Set[AgdsRow[A]]()
  var prev: Option[AgdsEntry[A]] = None
  var next: Option[AgdsEntry[A]] = None

  def process(forward: Boolean, backward: Boolean): Unit = {
    if (forward) {
      next.foreach { entry =>
        val dif = Math.abs(value - entry.value)
        val weight = 1d - (dif / column.range())
        entry.x += x * weight
        entry.process(forward = true, backward = false)
      }
    }

    if(backward){
      prev.foreach { entry =>
        val dif = Math.abs(value - entry.value)
        val weight = 1d - (dif / column.range())
        entry.x += x * weight
        entry.process(forward = false, backward = true)
      }
    }

    rows.foreach { row =>
      row.x += x * 1d / 5d //TODO:
    }
  }

  override def compare(that: AgdsEntry[A]): Int = value.compare(that.value)

  def connect(row: AgdsRow[A]): Unit = rows += row

  def disconnect(row: AgdsRow[A]): Unit = rows -= row
}

class AgdsColumn[A](discrete: Boolean) {
  def find(value: Double): AgdsEntry[A] = entries.find(_.value == value).get

  private var entries = SortedSet[AgdsEntry[A]]()

  def add(value: Double) {
    entries += AgdsEntry[A](value, this)
  }

  def build(): Unit = {
    if (!discrete) {
      entries.sliding(2).foreach { pair =>
        pair.head.next = Some(pair.last)
        pair.last.prev = Some(pair.head)
      }
    }
    entries.foreach(_.build())
  }

  def min(): Double = entries.head.value

  def max(): Double = entries.last.value

  def range(): Double = max() - min()

}

class Agds[A](rawRows: List[A])(implicit m: Manifest[A]) {
  def calculate(rawRow: A): Unit = {
    val row = rows.filter(_.row == rawRow).head
    row.x = 1
    row.process()
  }

  def getRows(): List[AgdsRow[A]] = {
    rows
  }

  private def getField(field: Field, row: A): Double = {
    field.setAccessible(true) // TODO: use scala compile macro
    field.getDouble(row)
  }

  private val columns: Map[String, AgdsColumn[A]]
  = m.runtimeClass.getDeclaredFields
    .map(field => field.getName ->
      new AgdsColumn[A](field.getType == classOf[Int]))
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
  val data = Random.shuffle(new Loader().loadIris()).slice(1,150)
  val agds = new Agds(data)
  agds.calculate(data.head)
  agds.getRows().sortBy(_.x).reverse.foreach(println(_))

  print(data.head)
}
