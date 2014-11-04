import org.scalatest.FunSuite
import com.ssjskipp.misc._

class RandomForestSpec extends FunSuite {

	test("Building a random forest") {
		// Stream some sine wave
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
	}

	test("EnsembleForest should deterministically work") {

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

		// Assert data returns expectation
		data.map(forest.process)
			.zip(expectation)
			.foreach(x =>
				assert(x._1 == x._2)
			)
	}

}