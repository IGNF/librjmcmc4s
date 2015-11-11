package fr.ign.rjmcmc4s.geometry.transform

import fr.ign.rjmcmc4s.rjmcmc.kernel.Transform
import fr.ign.rjmcmc4s.rjmcmc.kernel.TransformResult
import com.vividsolutions.jts.geom.Polygon
import com.vividsolutions.jts.geom.GeometryCollection
import com.vividsolutions.jts.triangulate.ConformingDelaunayTriangulationBuilder
import com.vividsolutions.jts.geom.Point
import com.vividsolutions.jts.geom.Coordinate

class PolygonTransform(val polygon: Polygon, val tolerance: Double = 0.1) extends Transform {
  val triangles = {
    val builder = new ConformingDelaunayTriangulationBuilder();
    builder.setSites(polygon);
    builder.setConstraints(polygon);
    builder.setTolerance(tolerance);
    val triangleCollection = builder.getTriangles(polygon.getFactory()).asInstanceOf[GeometryCollection];
    var areaSum = 0.0
    val trianglesInPolygon = (0 until triangleCollection.getNumGeometries).map(triangleCollection.getGeometryN(_).asInstanceOf[Polygon]).filter(p => {
      val area = p.getArea
      p.intersection(polygon).getArea() > 0.99 * area
    })
    trianglesInPolygon.map { triangle =>
      areaSum += triangle.getArea
      (areaSum, triangle)
    }
    //    List<Polygon> list = new ArrayList<Polygon>();
    //    totalArea = 0;
    //    for (int i = 0; i < triangleCollection.getNumGeometries(); i++) {
    //      Polygon triangle = (Polygon) triangleCollection.getGeometryN(i);
    //      // test
    //      double area = triangle.getArea();
    //      if (triangle.intersection(polygon).getArea() > 0.99 * area) {
    //        list.add(triangle);
    //        totalArea += area;
    //        System.out.println(triangle);
    //      }
    //    }
    //    this.triangles = list.toArray(new Polygon[list.size()]);
  }
  val totalArea = triangles.last._1
  override def apply(direct: Boolean, input: Iterable[Double]): TransformResult = {
    val iterator = input.iterator
    if (direct) {
      val s = iterator.next * totalArea
      val t = iterator.next
      val triangleIndex = triangles.indexWhere(s < _._1)
      val area = triangles(triangleIndex)._1
      val previousArea = if (triangles.isDefinedAt(triangleIndex - 1)) triangles(triangleIndex - 1)._1 else 0.0
      //val triangle = triangles.find(s < _._1)
      val triangle = triangles(triangleIndex)._2
      val tmp = Math.sqrt((s - previousArea) / (area - previousArea))
      val a = 1 - tmp
      val b = (1 - t) * tmp
      val c = t * tmp
      val coord = triangle.getCoordinates
      val p1 = coord(0)
      val p2 = coord(1)
      val p3 = coord(2)
      val x1 = p1.x
      val x2 = p2.x
      val x3 = p3.x
      val y1 = p1.y
      val y2 = p2.y
      val y3 = p3.y
      val x = a * x1 + b * x2 + c * x3;
      val y = a * y1 + b * y2 + c * y3;
      new TransformResult(Seq(x, y), 1.0 / totalArea)
    } else {
      val s = iterator.next
      val t = iterator.next
      val point = polygon.getFactory.createPoint(new Coordinate(s, t))
      val triangle = triangles.map(_._2).find(_.contains(point))
      //      if (triangle.isEmpty) throw Exception
      val coord = triangle.get.getCoordinates
      val p1 = coord(0);
      val p2 = coord(1);
      val p3 = coord(2);
      val e0 = p2.x - p1.x
      val e1 = p2.y - p1.y
      val f0 = p3.x - p2.x
      val f1 = p3.y - p2.y
      val v2 = (e1 * (s - p1.x) - e0 * (t - p1.y)) / (f0 * e1 - f1 * e0)
      val v1 = (t - p1.y - v2 * f1) / e1
      new TransformResult(Seq(v1 * v1, v2 / v1), totalArea)
    }
  }
  override def size = 2
}