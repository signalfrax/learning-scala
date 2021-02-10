import java.awt.geom.Point2D
import scala.annotation.tailrec
import scala.util.Random
import scala.io.StdIn

object ExerciseChapter1 extends App {

  case class Interval1D(min: Double, max: Double) {
    def length(): Double = max - min
    def contains(n: Double): Boolean = n >= min && n <= max
    def intersects(i: Interval1D): Boolean =
      if (length() > i.length())
        i.min >= min && i.min <= max || i.max >= min && i.max <= max
      else
        min >= i.min && min <= i.max || max >= i.min && max <= i.max
  }


  def ex121(n:Int): Unit = {

    def calculateDistances(p: List[Point2D.Double]): List[(Double, Point2D.Double, Point2D.Double)] = {

      @tailrec
      def calcDistance(p: Point2D.Double, l: List[Point2D.Double], distances: List[(Double, Point2D.Double, Point2D.Double)] = List()): List[(Double, Point2D.Double, Point2D.Double)] =
        l match {
          case List() => distances
          case head :: tail => calcDistance(p, tail, distances :+ (p.distance(head), p, head))
        }

      @tailrec
      def rec(l: List[Point2D.Double], distances: List[(Double, Point2D.Double, Point2D.Double)] = List()): List[(Double, Point2D.Double, Point2D.Double)] =
        l match {
          case List() => distances
          case head :: tail => rec(tail, calcDistance(head, tail, distances))
        }

      rec(p)

    }

    val r = new scala.util.Random
    val points = 1 to n map { _ =>
      new Point2D.Double(
        r.nextInt(100).toDouble,
        r.nextInt(100).toDouble
      )
    }
    pprint.pprintln(points)
    val distances = calculateDistances(points.toList).sortWith( (a, b) => a._1 < b._1)
    pprint.pprintln(distances)
    val closestDistance = distances
      .headOption
      .map{ case (distance, _, _) => distance }
      .getOrElse(0)

    println(closestDistance)

  }

  println("================== Distance between closest distance =======================")
  ex121(10)


  def ex122(n: Int): Unit = {

    def createIntervalFromString(str: String): Interval1D = {
      val pairs = str.split(",")
        .map(_.toDouble)
      Interval1D(
        min=pairs(0)
        ,max=pairs(1)
      )
    }

    @tailrec
    def readIntervals(n: Int, acc: List[Interval1D] = List()): List[Interval1D] =
      if (n == 0) acc
      else readIntervals(n - 1, acc :+ createIntervalFromString(StdIn.readLine()))

    def getIntersectingIntervals(intervals: List[Interval1D]): List[(Interval1D, Interval1D)] = {

      @tailrec
      def getIntersectingInterval(interval: Interval1D, intervals: List[Interval1D], acc: List[(Interval1D, Interval1D)] = List()): List[(Interval1D, Interval1D)] =
        intervals match {
          case List() => acc
          case fst :: Nil => if (interval.intersects(fst)) acc :+ (interval, fst) else acc
          case fst :: rst => getIntersectingInterval(interval, rst, if (interval.intersects(fst)) acc :+ (interval, fst) else acc)
        }

      @tailrec
      def rec(intervals: List[Interval1D], acc: List[(Interval1D, Interval1D)] = List()): List[(Interval1D, Interval1D)] =
        intervals match {
          case List() => acc
          case _ :: Nil => acc
          case fst :: rst => rec(rst, acc ++ getIntersectingInterval(fst, rst))
        }

      rec(intervals)

    }

    val intervals = readIntervals(n)
    val intersecting = getIntersectingIntervals(intervals)
    pprint.pprintln(intersecting)

  }

  println("================== Intersecting interval =======================")
  println("Please provide intervals in the following format 'min, max' e.g. 20.3, 30.2")
  ex122(3)

}
