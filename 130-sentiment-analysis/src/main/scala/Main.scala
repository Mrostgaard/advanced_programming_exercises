// Advanced Programming. Andrzej Wasowski. IT University
// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)
import org.apache.spark.ml.feature.Tokenizer
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.Row
import org.apache.spark.sql.SparkSession
import scala.collection.mutable.WrappedArray
import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.sql.types._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions.explode
import org.apache.spark.sql.functions.collect_set
import org.apache.spark.sql.functions._;
import org.apache.spark.ml.classification.MultilayerPerceptronClassifier
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator
import org.apache.spark.mllib.evaluation.MulticlassMetrics
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.util.MLUtils



object Main {

	type Embedding       = (String, List[Double])
	type ParsedReview    = (Integer, String, Double)

	org.apache.log4j.Logger getLogger "org"  setLevel (org.apache.log4j.Level.WARN)
	org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)
	val spark =  SparkSession.builder
		.appName ("Sentiment")
		.master  ("local[9]")
		.getOrCreate

  import spark.implicits._

	val reviewSchema = StructType(Array(
			StructField ("reviewText", StringType, nullable=false),
			StructField ("overall",    DoubleType, nullable=false),
			StructField ("summary",    StringType, nullable=false)))

	// Read file and merge the text abd summary into a single text column

	def loadReviews (path: String): Dataset[ParsedReview] =
		spark
			.read
			.schema (reviewSchema)
			.json (path)
			.rdd
			.zipWithUniqueId
			.map[(Integer,String,Double)] { case (row,id) => (id.toInt, s"${row getString 2} ${row getString 0}", row getDouble 1) }
			.toDS
			.withColumnRenamed ("_1", "id" )
			.withColumnRenamed ("_2", "text")
			.withColumnRenamed ("_3", "overall")
			.as[ParsedReview]

  // Load the GLoVe embeddings file

  def loadGlove (path: String): Dataset[Embedding] =
		spark
			.read
			.text (path)
      .map  { _ getString 0 split " " }
      .map  (r => (r.head, r.tail.toList.map (_.toDouble))) // yuck!
			.withColumnRenamed ("_1", "word" )
			.withColumnRenamed ("_2", "vec")
			.as[Embedding]



  def main(args: Array[String]) = {

		val glove  = loadGlove ("C:/Users/marku/Documents/glove.6B/glove.6B.300d.txt") // FIXME
		val reviews = loadReviews ("C:/Users/marku/Documents/reviews_Amazon_Instant_Video_5/Amazon_Instant_Video_5.json") // FIXME

    val tokenizer = new Tokenizer().setInputCol("text").setOutputCol("word")
    
    val tokenizedReviews = tokenizer.transform(reviews).drop("text")
    
    val labels = tokenizedReviews.withColumn("label", when($"overall" === 5.0 or $"overall" === 4.0, 2).when($"overall" === 3.0, 1).otherwise(0))
    
    val joinedReviews = tokenizedReviews.withColumn("word", explode($"word")).join(glove,"word").groupBy("id").agg(collect_set("vec").alias("vec"))
    
    val reviewVectors = joinedReviews.map(row => {
      val vecList:List[List[Double]] = row.getAs[WrappedArray[WrappedArray[Double]]](1).toList.map( arr => arr.toList)
      (row.getAs[Int](0), Vectors.dense(vecList.tail.foldLeft(vecList.head)((acc:List[Double], nextList:List[Double]) => acc.zip(nextList).map(t => t._1 + t._2)).map((x:Double) => x/vecList.length).toArray))
    }).withColumnRenamed("_1","id").withColumnRenamed("_2","features")
    
    val trainingData = labels.select("id","label").join(reviewVectors, "id")

    trainingData.show

    val splits = trainingData.randomSplit(Array(0.9, 0.1))//, seed = 1234L)
    val train = splits(0)
    val test = splits(1)
    
    
    val layers = Array[Int](300, 30, 10, 3)

    val trainer = new MultilayerPerceptronClassifier()
      .setLayers(layers)
      .setBlockSize(128)
      .setSeed(1234L)
      .setMaxIter(100)
    
    val model = trainer.fit(train)

    val result = model.transform(test)
    val predictionAndLabels = result.select("prediction", "label")

    val scoreAndLabels = predictionAndLabels.map(row => (row.getAs[Double](0),row.getAs[Int](1).toDouble)).rdd
    val evaluator = new MulticlassClassificationEvaluator()
      .setMetricName("accuracy")
    val metrics = new MulticlassMetrics(scoreAndLabels)
    println(s"Test set accuracy = ${evaluator.evaluate(predictionAndLabels)}")
    println(s"Weighted precision: ${metrics.weightedPrecision}")
    println(s"Weighted recall: ${metrics.weightedRecall}")
    println(s"Weighted F1 score: ${metrics.weightedFMeasure}")
    println(s"Weighted false positive rate: ${metrics.weightedFalsePositiveRate}")
    spark.stop
  }

}
