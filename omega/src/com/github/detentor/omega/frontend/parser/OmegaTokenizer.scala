package com.github.detentor.omega.frontend.parser

import scala.collection.mutable.ListBuffer


object OmegaTok
{
	def main(args: Array[String]) 
	{
		val t = new OmegaTokenizer
		t.tokenize("(  \"ok\" ok1 123456");
  
	}
}

class OmegaTokenizer 
{
//	def openParens : Parser[String] = "\\(".r
//	def closeParens : Parser[String] = "\\)".r
//	def openBracket = "\\{".r
//	def closeBracket = "\\}".r
//	def dotSymbol = "\\.".r ^^ { k => k}
//	def argSeparator : Parser[String] = ",".r
//	
//	//keywords:
//	def importKeyword = "import".r
//	
//	//Bloco Import
//	def importBlock = importKeyword~openBracket~closeBracket
//	
//	//Declaração de método
//	//tipo nomeMetodo(Int a, Int b) { }
//	def identifier = "[a-zA-Z][a-zA-Z0-9]*".r ^^ { k => k}
//	def classIdentifier = "[A-Z][a-zA-Z0-9]*".r ^^ { k => k}
//	
//	//definição de constantes
//	def numLiteral = "[0-9]+".r ^^ { k => NumConst(k) }
//	def stringLiteral = "\".*?\"".r ^^ { k => StringConst(k) }
//	def const : Parser[Const] = numLiteral | stringLiteral
	
	def tokenize(input : String) = 
	{
		val tokenList = ListBuffer.empty[Token]
		var curToken = ""
		var curIndex = 0
		
		while (curIndex < input.size)
		{
			val curChar = input.charAt(curIndex)
			
			val curToken = (curChar match
			{
				case '(' => OpenParensToken
				case ')' => CloseParensToken
				case '{' => OpenBracketToken 
				case '}' => CloseBracketToken
				case '.' => DotToken
				case ',' => ArgSeparatorToken
				case '"' => 
				{
					val newPos = collect(_ != '"')(input, curIndex + 1)
					if (newPos._2 >= input.size)
					{
						throw new IllegalArgumentException("Esperado: \". Encontrado: fim do fluxo de dados")
					}
					curIndex = newPos._2 //reduz porque ele será somado à frente
					new StringToken(newPos._1)
				}
				case ''' => CharToken
				case letter if Character.isLetter(letter) => 
				{
					val newPos = collect(Character.isLetterOrDigit)(input, curIndex)
					curIndex = newPos._2 - 1 //reduz porque ele será somado à frente
					new IdentifierToken(newPos._1)
				}
				case digit if Character.isDigit(digit) =>
				{
					val newPos = collect(Character.isDigit)(input, curIndex)
					curIndex = newPos._2 - 1 //reduz porque ele será somado à frente
					new NumberToken(newPos._1)
				}
				case _ => EmptyToken
			})
			
			if (curToken != EmptyToken) tokenList += curToken 
			curIndex += 1
		}
		tokenList
	}
	
	def collect(pred : Char => Boolean)(fromString : String, startPos : Int) = 
	{
		var newPos = startPos + 1
		while (newPos < fromString.size && 
					pred(fromString.charAt(newPos)))
		{
			newPos += 1
		}
		(fromString.substring(startPos, newPos), newPos)
	}
	
	def peekNext(input : String, inPos : Int) = 
	{
		if (inPos < input.size) Some(input.charAt(inPos)) else None
	}
	
	trait Token
	{
		def getValue : String
	}
	
	object EmptyToken extends Token{ def getValue = "(" }
	object OpenParensToken extends Token{ def getValue = "(" }
	object CloseParensToken extends Token{ def getValue = ")" }
	object OpenBracketToken extends Token{ def getValue = "{" }
	object CloseBracketToken extends Token{ def getValue = "}" }
	object DotToken extends Token{ def getValue = "." }
	object ArgSeparatorToken extends Token{ def getValue = "," }
//	object DoubleQuoteToken extends Token{ def getValue = "\"" }
	object CharToken extends Token{ def getValue = "'" }
	
	case class IdentifierToken(value : String) extends Token
	{ 
		def getValue = value
		override def toString = "Identifier: " + getValue
	}
	
	case class StringToken(value : String) extends Token
	{ 
		def getValue = value
		override def toString = "String:[" + getValue + "]"
	}
	
	case class NumberToken(value : String) extends Token
	{ 
		def getValue = value
		override def toString = "Number: " + getValue
	}
	

}