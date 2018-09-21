package streamingProcessing

/**
  * Created by LENOVO on 2017/3/26.
  */
import org.apache.log4j.{Level, Logger}
import org.apache.spark.internal._

object StreamingExample {
  def setStreamingLogLevel(): Unit ={
    val log4jInitialized = Logger.getRootLogger.getAllAppenders.hasMoreElements
    if (!log4jInitialized){
//      logInfo("Setting log levele to [WARN] for streaming example."+
//      " To override add a custom log4j properties to the classPath.")
      Logger.getRootLogger.setLevel(Level.WARN)
    }
  }
}
