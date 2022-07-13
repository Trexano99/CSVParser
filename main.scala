package csv

import java.io.FileNotFoundException
import scala.io.Source.fromFile
import scala.util.{Failure, Success, Try}


object CsvParserCLI {
  def main(args: Array[String]){
  	args.foreach { src =>
	  val input: String = Try(fromFile(src)) match {
	    case Success(file) => file.mkString
	    case Failure(e)    => throw new FileNotFoundException()
	  }

	  val p = new CsvParser(input)

	  p.parse match {
	    case p.Success(result, _) => result.printCsv()  
	    case p.Failure(x, _) => println(x)
	    case p.Error(x,_) => println(x)
	    }
	}
  }

  
    
}

