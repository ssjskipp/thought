import org.scalatest.FunSuite
import com.ssjskipp.misc._

class RandomForestSpec extends FunSuite {

	test("Random forests should work") {

		val classifyRootA = SplitNode(
			(x: Seq[Double]) => x(0) > 5,
			
			ClassifyLeaf((x: Seq[Double]) =>
				if (x(1) > 0) "Positive" else "Negative or Zero"),
			
			ClassifyLeaf((x: Seq[Double]) =>
				if (x(1) < 0) "Negative" else "Positive or Zero")
		)
	
		val classifyRootB = SplitNode(
			(x: Seq[Double]) => x(0) < -5,
			
			ClassifyLeaf((x: Seq[Double]) =>
				if (x(1) > 0) "Positive" else "Negative or Zero"),
			
			ClassifyLeaf((x: Seq[Double]) =>
				if (x(1) < 0) "Negative" else "Positive or Zero")
		)
	
		val forest = new EnsembleForest[String](
			// Trees
			Seq(new DecisionTree[String](classifyRootA),
				new DecisionTree[String](classifyRootB)),
			// Weights
			Seq(0.40, 0.60)
			)

		val data = Seq(
			Seq(0d, 0d),		// R, R, 0
			Seq(3d, -1d),		// R, R, neg
			Seq(-6d, -1d),	// R, L, neg
			Seq(6d, 2d),		// L, R, pos
			Seq(-6d, 4d)		// R, L, pos
		)

		val expectation = Seq(
			"Positive or Zero",
			"Negative",
			"Negative or Zero",
			"Positive or Zero",
			"Positive"
		)

		data.map(forest.process)
			.zip(expectation)
			.foreach(x =>
				assert(x._1 == x._2)
			)
	}

	test("Trivial classification tree should give expected results") {
		
		val leftDecide = ClassifyLeaf(
			(x: Seq[Double]) => if (x(1) > 0) "Positive" else "Negative or Zero"
		)
		
		val rightDecide = ClassifyLeaf(
			(x: Seq[Double]) => if (x(1) < 0) "Negative" else "Positive or Zero"
		)

		val root = SplitNode(
			(x: Seq[Double]) => x(0) > 5,
			leftDecide,
			rightDecide
		)

		val tree = new DecisionTree[String](root)

		val sortRight = Seq(
			Seq(0d, 15d),
			Seq(1d, -15d),
			Seq(-10d, -10d),
			Seq(5d, 0d)
		)

		val answerRight = Seq(
			"Positive or Zero",
			"Negative",
			"Negative",
			"Positive or Zero"
		)

		val sortLeft = Seq(
			Seq(15d, 10d),
			Seq(5.001d, -15d),
			Seq(7d, 0d)
		)

		val answerLeft = Seq(
			"Positive",
			"Negative or Zero",
			"Negative or Zero"
		)

		val testData = sortLeft ++ sortRight
		val expectation = answerLeft ++ answerRight

		val results = testData.map(tree.process)

		results.zip(expectation).foreach(
			x => assert(x._1 == x._2)
		)
	}

}