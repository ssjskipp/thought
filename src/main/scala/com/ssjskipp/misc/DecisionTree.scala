package com.ssjskipp.misc;

import scala.annotation.tailrec;

/**
 * Base node for the trees. Acts on a feature returning an output
 */
abstract class Node[+T] {
	val act: Seq[Double] => T
	def apply(x: Seq[Double]):T = act(x)
}

// The internal node of a decision tree.
// Up to the tree to decide which path to take based on act.
case class SplitNode(act: Seq[Double] => Boolean,
	left: Node[Any], right: Node[Any]) extends Node[Boolean]

/**
 * A decision tree is defined such that each internal node splits (deterministically)
 * feature data input to a left or right node, and each leaf calculate and return some
 * result on the feature
 * 
 *
 * A classification tree might be typed to strings,
 * a regression tree might use Double, whatever.
 */
class DecisionTree[T](root: Node[Any]) {
	def process(x: Seq[Double]): T = {
		@tailrec
		def classifyFrom(x: Seq[Double], from: Node[Any]): T = {
			 from match {
			 	case internal: SplitNode => classifyFrom(x,
			 		if (internal(x))
			 			internal.left
			 		else
			 			internal.right)
			 	case leaf: Node[T] => leaf(x)
			 }
		}

		classifyFrom(x, root)
	}

	def apply(x: Seq[Double]) = process(x)
}

case class GenericLeaf[T](act: Seq[Double] => T) extends Node[T]
case class ClassifyLeaf(act: Seq[Double] => String) extends Node[String]
case class RegressionLeaf(act: Seq[Double] => Double) extends Node[Double]

object TreeBuilder {

	/**
	 * Build a tree from a dataset,
	 * A leaf-node-action-provider
	 * 	-> Function that takes a set of 'like' data and gives a LeafFunction
	 * 		-> LeafFunction: Takes a data point and responds with a type of T 
	 *  (takes a clustered set of data and gives a function for that leaf)
	 * A split-node-action-provider
	 * 	-> Function that takes a set of 'like' data and gives a SplitFunction
	 * 		-> SplitFunction: Classifies a data point into one of 2 classes
	 * @type {DecisionForest[T]}
	 */
	def buildTree[T](data: Seq[Seq[Double]],
		leafActProvider: (Seq[Seq[Double]] => (Seq[Double] => T)),
		splitAct: (Seq[Seq[Double]] => (Seq[Double] => Boolean)),
		params: Option[Map[String, Any]]) = {
		val defaults = Map("maxDepth" -> 5, "minNodeSize" -> 5)
		val settings = params.getOrElse(Map[String, Any]())
			.orElse(defaults)

		val maxDepth = settings("maxDepth").asInstanceOf[Int]
		val minLength = settings("minNodeSize").asInstanceOf[Int]

		def step(data: Seq[Seq[Double]], depth: Int = 0): Node[Any] = {
			if (data.length < minLength || depth >= maxDepth) {
				// Leaf
				GenericLeaf[T](leafActProvider(data))
			} else {
				// Split node
				val split = splitAct(data)
				val left = data.filter(x => split(x))
				val right = data.filter(x => !split(x))
				
				SplitNode(split,
					step(left, depth + 1),
					step(right, depth + 1))
			}
		}

		val randomTreeRoot = step(data)

		// @TODO: Train/optimize this tree before returning.
		
		new DecisionTree[T](randomTreeRoot)
	}
}