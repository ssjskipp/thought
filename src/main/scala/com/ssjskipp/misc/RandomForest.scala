package com.ssjskipp.misc;

import scala.annotation.tailrec;

/**
 * A tree builder can construct a decision tree from an ordered sequence of data
 * and matching 'truth' values of decision type T, matching the data.
 *
 * An optional parameter map can be provided.
 */
trait ForestBuilder {
	def makeForest[T](data: Seq[Seq[Double]],
		truth: Seq[T],
		params: Option[Map[String, String]]): Iterable[DecisionTree[T]];
}

/**
 * Collection of weighted decision trees of a shared type for ensemble voting
 */
class EnsembleForest[T](trees: Seq[DecisionTree[T]], weights: Seq[Double]) {

	def process(x: Seq[Double]): T = {
		val votes = trees							// For each tree
			.map(tree => tree(x))				// Apply the tree to data
			.zip(weights)								// Zip with weights
			.groupBy(_._1)							// Group by the result
			.mapValues(x => {						// Reduce group values to vote
				x.map(y => y._2)					// Project vote value out of tuple
				.reduce((a, b) => a + b)	// Sum votes for result class
			})

		// Grab class with maximum vote value
		// Then project result out
		votes.maxBy(x => x._2)
			._1
	}

	def apply(x: Seq[Double]) = process(x)
}


/**
 * Builder for random forests.
 */
object RandomForest extends ForestBuilder {
	val rand = new scala.util.Random()

	/**
	 * params
	 * 	T - number of trees
	 * 	D - max depth
	 * 	p - amount of randomness
	 * 	w - weak learner model
	 * 	t - training objective function
	 * 	o - feature transform
	 */
	override def makeForest[T](data: Seq[Seq[Double]],
		truth: Seq[T],
		params: Option[Map[String, String]] = None): Iterable[DecisionTree[T]] = {
		
		val numTrees = params.getOrElse(Map("T" -> "10")).getOrElse("T", "10").toInt
		val maxDepth = params.getOrElse(Map("D" -> "5")).getOrElse("D", "5").toInt
		
		(0 until numTrees).flatMap { i =>
			val dataSubset = data.zipWithIndex
				.map(x => (rand.nextDouble <= 0.33, x))
				.groupBy(x => x._1)

			Seq(makeTree[T](dataSubset(true).map(_._2._1), maxDepth))
		}
	}

	private def makeTree[T](data: Seq[Seq[Double]],
		maxDepth: Int) = {

		val minLength = 5;

		def step(data: Seq[Seq[Double]], depth: Int = 0): Node[Any] = {
			if (data.length < minLength || depth >= maxDepth) {
				// Leaf
				RegressionLeaf(regressOn(data))
			} else {
				// Split node
				val split = splitOn(data)
				val left = data.filter(x => split(x))
				val right = data.filter(x => !split(x))
				
				SplitNode(splitOn(data),
					step(left, depth + 1),
					step(right, depth + 1))
			}
		}
		
		new DecisionTree[T](step(data))
	}

	// Bluh, averaging stuff is dumb
	private def regressOn(data: Seq[Seq[Double]]): Seq[Double] => Double = {
		val len = data.length
		val res = data.map(_(1)).sum / len

		(x: Seq[Double]) => res
	}

	// Provides a function that decides, given a data set, how to split on a feature
	private def splitOn(data: Seq[Seq[Double]]): Seq[Double] =>	Boolean = {
		val dim = rand.nextInt(data(0).size)
		val min = data.minBy(x => x(dim)).apply(dim)
		val max = data.maxBy(x => x(dim)).apply(dim)
		val split = (max - min) / 2
		(x: Seq[Double]) => x(dim) >= split
	}
}
