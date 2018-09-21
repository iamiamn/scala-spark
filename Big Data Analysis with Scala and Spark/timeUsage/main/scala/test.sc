import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{SparkSession, functions, _}
import org.apache.spark.sql._
org.apache.spark.sql.expressions.scalalang.typed
import org.apache.spark.sql.types._
import spark.implicits
//import org.apache.spark.{SparkConf, SparkContext}
//val conf = new SparkConf().setMaster("local[*]").setAppName("myApp")
//val sc = new SparkContext(conf)

//for sqlContext, we need to use sparkSession.
val spark: SparkSession =
SparkSession
  .builder()
  .appName("myApp")
  .config("spark.master", "local[*]")//config("spark.master", "spark://192.168.56.1:7077")
  .getOrCreate()

//directly create Dataset
val sqlContext = spark.sqlContext
val ds: Dataset[String] = sqlContext.read.textFile("file:///d:/text.txt")

val wordRDD = spark.sparkContext.textFile("file:///d:/text.txt")

val dfSchema = StructType(StructField("word", StringType, nullable = false)::Nil)//we need to pass a sql[StructField into StructType constructor
val rawData: RDD[Row] = wordRDD.flatMap(_.split(" ")).map(str => Row(str.trim.toUpperCase))
val df = spark.createDataFrame(rawData,dfSchema)
df.show
val wordCountDF = df.groupBy(new Column("word")).agg("word".sum.as("count"))

