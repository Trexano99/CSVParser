
package csv

case class CsvRapr (private val columnsNames : List[String], private val rowsContent : List[List[String]]) {
	
	def printCsv() = {
	   /*
	   TESTING PART
	   println("Size colonne: "+columnsSize)
	   println("Nomi colonne : ") ;
	   printList(columnsNames)
	   println("Righe : ") ;
	   rowsContent.foreach {riga => printList(riga)};
	   */
	   println(separationLine)
	   println(line(columnsNames))
	   println(separationLine)
	   rowsContent.foreach {riga => println(line(riga))}
	   println(separationLine)
	}
	
	private var columnsSize : List[Int] = getMaxColumnsSize( rowsContent.foldLeft(List[List[Int]]()) {(lista, riga) => lista :+ getRowSize(riga)} :+ getRowSize(columnsNames))

	private def getRowSize (contenutoRiga : List[String]) : List[Int] = contenutoRiga.foldLeft(List[Int]()){(lista, cella) => (lista :+ (cella.size))}	
	
	private def getMaxColumnsSize(grandezzeLinee : List[List[Int]]) = grandezzeLinee.tail.foldLeft(grandezzeLinee.head) {(precGrand, newLine) => mergeWithMax(precGrand, newLine)}	
	
	private def mergeWithMax(rigaUno : List[Int], rigaDue : List[Int]) = rigaUno.zip(rigaDue).foldLeft(List[Int]()) {(lista, elemento) => lista :+ elemento._1.max(elemento._2)}
	
	
	private def separationLine : String = columnsSize.foldLeft("-") {(stringaF, grandL) => stringaF + ("-" * (grandL+3))}
	
	private def line (rowContent : List[String]) : String =  rowContent.zip(columnsSize).foldLeft("|") {(stringaF, contenuto) => stringaF +" " + adattaStringa(contenuto._1, contenuto._2)+ " |"}

	private def adattaStringa (stringa : String, lunghezza : Int) : String = 	
	   stringa + (" " * (lunghezza - stringa.length))
	   

	//FOR TESTING
	private def printList(lista : List[String]) = {
		println("La lista contiene: ")
		for(element <- lista){
			println(element)
		}
	}
}

