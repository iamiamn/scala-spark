import org.apache.spark.sql._

/**
  * Created by LENOVO on 2017/3/26.
  */
object explanation {
  import org.apache.spark.rdd.RDD
  import java.io._

  import org.apache.spark.sql.types._
  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._
  //just import org.apache.spark.sql can't work

  //for sqlContext, we need to use sparkSession.
  //need import org.apache.spark.sql.SparkSession
  val spark: SparkSession =
  SparkSession
    .builder()
    .appName("myApp")
    .config("spark.master", "local[*]")//config("spark.master", "spark://192.168.56.1:7077")
    .getOrCreate()

//  //directly create Dataset
//  val sqlContext = spark.sqlContext
//  val ds: Dataset[String] = sqlContext.read.textFile("file:///d:/text.txt")

  val wordRDD = spark.sparkContext.textFile("file:///d:/text.txt")
  val wordCountRDD = wordRDD.flatMap(_.split(" ")).map(str =>(str.trim.toUpperCase, 1)).reduceByKey(_+_)

  val dfSchema = StructType(StructField("word", StringType, nullable = false)::Nil)//we need to pass a sql[StructField into StructType constructor
  //class Row need import org.apache.spark.sql.Row
  val rawData: RDD[Row] = wordRDD.flatMap(_.split(" ")).map(str => Row(str.trim.toUpperCase))
  val df = spark.createDataFrame(rawData,dfSchema)

  //as[String] transform DataFrame into dataSets, but it needs first create spark(new SParkSession), then
  //import spark.implicts._
  import spark.implicits._
  val ds: Dataset[String] = df.as[String]
  //  val word = new Column("word")
  val wordCountDF: DataFrame = df.groupBy("word").count//this will create a new column called count

  //count function need to
  //import org.apache.spark.sql.functions._
  val wordCountDS = ds.groupBy($"word").agg(count("word").as[Int].as("count"))
  def main(args: Array[String]):Unit = {


    //    df.show//one column
    //one way to save csv
    //    wordCountDF.write.option("header", true).csv("d://file1.csv")
    // make sure the file does not exist

    wordCountDS.write.format("com.databricks.spark.csv").save("d://file33.csv") //this expression will create new file,
    //    wordCountDF.write.text("d://file1")
    wordCountRDD.map(println)
  }
}
object creatDF{
  import org.apache.spark.sql.SparkSession
  val spark = SparkSession.builder().appName("myDFcreater").config("spark.master", "local[*]").getOrCreate()
  val rawRDD = spark.sparkContext.parallelize(Seq((1,1,"a"), (1,2,"b"),(1,3,"c")))
  import spark.implicits._//once import implict transformation, we can use toDF on tupleRDD
  val df = rawRDD.toDF("Groupid","id", "name")
  val ds = rawRDD.toDS
  //DF to DS
  // first import spark.implicits._
  //second, use as and assgin the data type
  val ds2 = df.as[(Int, Int, String)]
  def main(args: Array[String]): Unit ={
    df.show//column names: Groupid, id, name
    ds.show //column names : _1 _2 _3
    val ds3 = ds.withColumnRenamed("_1", "GroupId")//rdd is an read only, partitioned collection of records
    ds2.show//column names: Groupid, id, name
    ds3.show//column names: Groupid, _2, _3

  }

}
