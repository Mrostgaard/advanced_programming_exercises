// Advanced Programming. Andrzej Wasowski. IT University
// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)

import org.apache.spark.ml.feature.Tokenizer
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions.explode
import org.apache.spark.sql.functions.collect_set


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

    val glove  = loadGlove ("C:/Users/marku/Documents/glove.6B/glove.6B.100d.txt") // FIXME
    val reviews = loadReviews ("C:/Users/marku/Documents/reviews_Amazon_Instant_Video_5/Amazon_Instant_Video_5.json") // FIXME

    // replace the following with the project code
    glove.show
    val tokenizer = new Tokenizer().setInputCol("text").setOutputCol("word")
    val tokenizedReviews = tokenizer.transform(reviews).drop("text")
    
    val joinedReviews = tokenizedReviews.withColumn("word", explode($"word")).join(glove,"word").groupBy("id").agg(collect_set("vec").alias("vec"))
    //joinedReviews.collect().map((id:Int,vecList:List[List[Double]]) => vecList.tail.foldLeft(vecList.head)((acc:List[Double], nextList:List[Double]) => (acc, nextList).zipped.map(_ + _)))
    

		spark.stop
  }

}
