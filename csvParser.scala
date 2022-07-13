package csv

import scala.language.implicitConversions
import scala.util.parsing.combinator._
import scala.util.matching._
import scala.util.Try


class CsvParser(private val input: String) extends JavaTokenParsers{


	override def skipWhitespace : Boolean = false
	
	def parse : ParseResult[CsvRapr] = parseAll(rootParser, input)
	
	private def rootParser : Parser[CsvRapr] = line ~ rep(line) ^^ {case titoli ~ righe => CsvRapr(titoli, righe)} 
	
	private def line : Parser[List[String]] = repsep(cella, ",") <~ ("""[\n]""".r) 
	
	private def cella : Parser[String] = """[^,^\n]*""".r 

}
