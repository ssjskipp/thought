package com.ssjskipp.thought;

import java.io.FileWriter
import com.ssjskipp.misc._
import spray.json._
import DefaultJsonProtocol._

object main extends App {
	val dest = new FileWriter("output/test.json");
	
	def dataSource(f: Double => Double, step: Double): Stream[Seq[Double]] = {

		def sample(theta: Double, f: Double => Double): Stream[Seq[Double]] =
			Seq(theta, f(theta)) #:: sample(theta + step, f)
		
		sample(0, f)
	}

	val source = dataSource((x: Double) => Math.sin(x), Math.PI / 64).iterator
	val featureSet = (0 until 1000).map(x => source.next())

	// Build a forest with the (x, y) pair as the feature set
	// and the (y) as truth results
	println(featureSet.take(10))

	val forest = RandomForest.makeForest[Double](featureSet, featureSet.map(_(1)), Some(Map("T" -> "25")))

	// Run some data
	val res = (-100 until 100).map(x => (x, forest(Seq(x)))).toJson

	dest.write(res.compactPrint)

	dest.flush()
}
