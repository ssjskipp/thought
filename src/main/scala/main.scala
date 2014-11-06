package com.ssjskipp.thought;

import java.io.FileWriter
import com.ssjskipp.misc._

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
	val forest = RandomForest.makeForest[Double](featureSet, featureSet.map(_(1)), Some(Map("T" -> "25")))

	// See what the forest can spit out
	for (i <- 1 until 10) {
		val theta = i * (Math.PI / 2)
		println(
			forest( Seq(theta, Math.sin(theta)) )
		)
	}
}
