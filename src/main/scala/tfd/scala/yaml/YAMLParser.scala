import scala.util.parsing.combinator._
import scala.util.matching.Regex  

package tfd.scala.yaml {

  object YAMLParser extends RegexParsers {
	  override def skipWhitespace = false
    
	  val mapSeparator = ": *\r?\n?"r
     
      val seqIndicator = "- *\r?\n?"r
   
	  val scalarData = """[^,\r\n\}\{]+"""r
     
      val mappingKey = """[^-:\r\n\}\{]+"""r
     
      val newLine = " *\r*\n+"r
     
      val inlineSeparator = " *, +"r
     
      val openBrace = "\\{ *"r;
   
      val closeBrace = " *\\}"r;
      
      val openBracket = "\\[ *"r;
   
      val closeBracket = " *\\]"r;
     
      def leadingSpaces(numLeadingSpaces:Int):Parser[String] = ("^ {" + numLeadingSpaces + ",}"r)  
     
	  def mappings(numLeadingSpaces:Int):Parser[Map[String, Any]] =
	        ( indentedMap(numLeadingSpaces) | inlineMap) ^^ { Map() ++ _ } 
            
      def indentedMap(numLeadingSpaces:Int):Parser[List[(String, Any)]] = rep1sep(indentedMapping(numLeadingSpaces), newLine)
       
      def inlineMap:Parser[List[(String, Any)]] = (openBrace ~> rep1sep(inlineMapping, inlineSeparator) <~ closeBrace)
      
      def indentedMapping(numLeadingSpaces:Int):Parser[(String, Any)] = 
        leadingSpaces(numLeadingSpaces) ~> mappingKey ~ mapSeparator ~ (list(0) | mappings(numLeadingSpaces+1) | scalarElement) ^^ { case key~_~value => (key, value) }
      
      def inlineMapping:Parser[(String, Any)] = 
    	   mappingKey ~ mapSeparator ~ (inlineList | inlineMap | scalarElement) ^^ { case key~_~value => (key, value) }
           
      def list(numLeadingSpaces:Int):Parser[List[Any]] =
            (inlineList | indentedList(numLeadingSpaces)) ^^ { List() ++ _ }
      
      def indentedList(numLeadingSpaces:Int):Parser[List[Any]] =
    	  rep1sep( leadingSpaces(numLeadingSpaces) ~ seqIndicator ~>nestedData(numLeadingSpaces), newLine)
      
      def inlineList:Parser[List[Any]] =
            (openBracket ~> rep1sep( nestedData(0), inlineSeparator) <~ closeBracket)
             
      def nestedData(numLeadingSpaces:Int):Parser[Any] = (list(numLeadingSpaces+1) | mappings(0) | scalarElement) 
     
      def scalarElement:Parser[String] = scalarData  ^^ { case value => value.trim }

	  def yaml:Parser[Any] = ( opt(newLine) ~> list(0) | opt(newLine) ~> mappings(0) )
   
	  def parse(text : String):ParseResult[Any] = {
		parse(yaml, text)
	  }
  }
  
  object Main {
	  def main(args:Array[String]) {
		  val input = YAMLParser.parse(
"""    
{ key1: value1, key2: value2 }
""")
    System.out.println(input)
	  }	  
  }
  
}