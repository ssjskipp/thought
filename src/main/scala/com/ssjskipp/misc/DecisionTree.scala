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

// A couple useful base leaf definition types
case class ClassifyLeaf(act: Seq[Double] => String) extends Node[String]
case class RegressionLeaf(act: Seq[Double] => Double) extends Node[Double]
