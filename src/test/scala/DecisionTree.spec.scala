import org.scalatest.FunSuite
import com.ssjskipp.misc._

class DecisionTreeSpec extends FunSuite {

	test("Trivial DecisionTree should give expected results") {
		
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