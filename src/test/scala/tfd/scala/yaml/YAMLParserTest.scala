package tfd.scala.yaml

import junit.framework._
import org.junit.Assert._

class YAMLParserTest extends TestCase {
  import YAMLParser._

  def testSimpleMap {
     assertEquals(Map("key"->"value"), parse("""key: value""").get)
     assertEquals(Map("key1"->"value1", "key2"->"value2"), parse(
"""
key1: value1
key2: value2
""").get)
     assertEquals(Map("key 1"->"value 1", "key 2"->"value 2"), parse(
"""
key 1: value 1
key 2: value 2
""").get)
  }
  
  def testSimpleList { 
    assertEquals(List("item1"), parse(
"""
- item1 
""").get)
    assertEquals(List("item1", "item2"), parse(
"""
- item1
- item2
""").get)
  }

  
  def testListOfList {
     assertEquals(List(
    				List("item11", "item12"),
    			    List("item21", "item22")),
parse(
"""
- 
  - item11
  - item12
- 
  - item21
  - item22
""").get)
  }
  
  def testMapOfMap {
	  assertEquals(Map("JFrame" -> Map("name" -> "myFrame", "title" -> "My App Frame")),
parse(
"""    
JFrame:
     name: myFrame
     title: My App Frame
""").get)    
  }
  
    def testListOfMap {
    	assertEquals(List(
    				Map("name"-> "John Smith", "age"->"33"),
    			    Map("name"-> "Mary Smith", "age"->"27")),
parse(
"""
- name: John Smith
  age: 33
- name: Mary Smith
  age: 27
""").get)
  }
  
    def testMapOfList {
    	assertEquals(Map("men" -> List("John Smith", "Bill Jones"), "women" -> List("Mary Smith", "Susan Williams")),
parse(
"""    
men: 
  - John Smith
  - Bill Jones
women:
  - Mary Smith
  - Susan Williams
""").get)    
  }
    
   def testInlineMap {
     assertEquals(Map("key"->"value"), parse("""key: value""").get)
     assertEquals(Map("key1"->"value1", "key2"->"value2"), parse(
"""{ key1: value1, key2: value2 }""").get)
   }
   
     def testMapOfMapOfMap {
	  assertEquals(Map("JFrame" -> Map("content" -> Map("button" -> "press"))),
parse(
"""    
JFrame:
     content: 
     button: press

""").get)    
  }
     
      def testMapOfInlineList {
    	assertEquals(Map("men" -> List("John Smith", "Bill Jones"), "women" -> List("Mary Smith", "Susan Williams")),
parse(
"""    
men: [ John Smith, Bill Jones ]
women: [ Mary Smith, Susan Williams ]
""").get)    
  }
  
}
