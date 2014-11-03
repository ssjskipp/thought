package com.ssjskipp.misc;

import scala.annotation.tailrec;

/**
 * Each tree:
 * Filter data through a series of binary decisions (f(x) => x', subset of x)
 * Leaf nodes contain a classifier or regresser (f(x) => result)
 *
 * Forest of trees:
 * Pass a test through all trees, 
 */

/**
 * Base node for the trees. Acts on a feature returning an output
 */
abstract class Node[+T] {
	val act: Seq[Double] => T
	def apply(x: Seq[Double]):T = act(x)
}

// Case classes for different node types
case class SplitNode(act: Seq[Double] => Boolean,
	left: Node[Any], right: Node[Any]) extends Node[Boolean]
case class ClassifyLeaf(act: Seq[Double] => String) extends Node[String]
case class RegressionLeaf(act: Seq[Double] => Double) extends Node[Double]


/**
 * A decision tree is defined by a root node and can process a node
 * to whatever the tree's leafs decide to do.
 *
 * A classification tree might be typed to strings,
 * a regression tree might use Double, whatever.
 */
class DecisionTree[T](root: SplitNode) {
	def process(x: Seq[Double]): T = {
		@tailrec
		def classifyFrom(x: Seq[Double], from: SplitNode): T = {
			 val next = if (from(x)) from.left else from.right

			 next match {
			 	case internal: SplitNode => classifyFrom(x, internal)
			 	case leaf: Node[T] => leaf(x)
			 }
		}

		classifyFrom(x, root)
	}

	def apply(x: Seq[Double]) = process(x)
}

/**
 * Collection of decision trees of a shared type for ensemble learning
 *
 * Builds forests from feature samples 
 */
class EnsembleForest[T](trees: Seq[DecisionTree[T]], weights: Seq[Double]) {
	// lazy val trees = Seq[DecisionTree[T]]()
	// lazy val weights = Seq[Double]()

	def makeTree(data: Iterable[Seq[Double]], truth: Seq[T]) = {
		???
	}

	def process(x: Seq[Double]): T = {
		val votes = trees							// For each tree
			.map(tree => tree(x))				// Apply the tree to data
			.zip(weights)								// Zip with weights
			.groupBy(_._1)							// Group by the result
			.mapValues(x => {						// Reduce group values to vote
				x.map(y => y._2)					// Project vote value out of tuple
				.reduce((a, b) => a + b)	// Sum votes for class
			})

		println(votes)

		// Grab minimmum from result votes + project result out
		votes.maxBy(x => x._2)
			._1
	}

	def apply(x: Seq[Double]) = process(x)
}


object RegressionForest {

}
