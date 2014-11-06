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
		params: Option[Map[String, String]]): EnsembleForest[T];
}

/**
 * Collection of weighted decision trees of a shared type for ensemble voting
 * Can use a grouping function to better group tree results (perhaps rounding?)
 */
class EnsembleForest[T](trees: Seq[DecisionTree[T]], weights: Seq[Double],
	clusterFunc: (T => String)) {

	def process(x: Seq[Double]): T = {
		val cluster = trees						// Each tree is a vote to cluster
			.map(tree => tree(x))				// Apply the tree to data
			.zip(weights)								// Zip with weights
			.groupBy[String](						// Group by the result of the grouping function
				d => clusterFunc(d._1))
		
		val votes = cluster
			.mapValues(x => {						// Reduce each group's values to count votes
				x.map(y => y._2)					// Project vote value out of tuple
				.reduce((a, b) => a + b)	// Sum votes for result class
			})

		val winningCluster = votes.zipWithIndex.maxBy(x => x._1._2)

		// For now, give head of winning cluster
		cluster(winningCluster._1._1)(0)._1		// I'M SORRY FOR ALL THIS PROJECTION
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
		params: Option[Map[String, String]] = None): EnsembleForest[T] = {
		
		val numTrees = params.getOrElse(Map("T" -> "10")).getOrElse("T", "10").toInt
		val maxDepth = params.getOrElse(Map("D" -> "5")).getOrElse("D", "5").toInt

		// Divide test set into 2 parts, one to build forest (true), one to train forest (false)
		val dataSplit = data.zipWithIndex
				.map(x => (rand.nextDouble <= 0.66, x))
				.groupBy(x => x._1)

		// Make numTrees trees
		val trees: Seq[DecisionTree[T]] = (0 until numTrees).flatMap { i =>
			
			// Divide into 2 sets, just like the forest
			val treeDataSplit = dataSplit(true).map(_._2._1)
				.zipWithIndex
				.map(x => (rand.nextDouble <= 0.66, x))
				.groupBy(x => x._1)

			// Build tree with the 'true' split
			val tree = TreeBuilder.buildTree[T](
				treeDataSplit(true).map(_._2._1),
				regressLeaf,
				dumbSplit,
				Some(Map("maxDepth" -> 5))
			)

			// Train tree with the 'false' split
			// @TODO: Train tree


			// Return
			Seq(tree)
		}

		// Train the collection of trees with the 'false' split (1/3)
		// @TODO: Weight forest
		val weights = (0 until trees.length).map(x => 1d).toSeq

		val doubleCluster = (x: T) => {
			(x.asInstanceOf[Double]).toString
		}

		// Return the new random forest
		new EnsembleForest[T](trees,
			weights,
			doubleCluster)
	}

	// Bluh, averaging stuff is dumb
	// I'm averaging each point left over
	// Assumes last dimension is 'response' value
	// @TODO: Make this better
	def regressLeaf[T](data: Seq[Seq[Double]]): Seq[Double] => T = {
		val len = data.length
		val res = data.map(_.last).sum / len // Project tail value

		// @Todo, make this an actual thing.
		(x: Seq[Double]) => res.asInstanceOf[T]
	}

	// Provides a function that decides, given a data set, how to split on a feature
	// Currently divides extent of a random dimension in half
	// then splits on that dimension
	def dumbSplit[T](data: Seq[Seq[Double]]): Seq[Double] =>	Boolean = {
		val dim = rand.nextInt(data(0).size - 1)
		val min = data.minBy(x => x(dim)).apply(dim)
		val max = data.maxBy(x => x(dim)).apply(dim)
		val split = (max - min) / 2
		(x: Seq[Double]) => x(dim) <= split
	}
}
