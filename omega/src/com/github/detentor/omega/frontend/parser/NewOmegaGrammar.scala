package com.github.detentor.omega.frontend.parser

import scala.util.parsing.combinator.RegexParsers
import com.github.detentor.omega.frontend.parser.ast.NumConst
import com.github.detentor.omega.frontend.parser.ast.StringConst
import com.github.detentor.omega.frontend.parser.ast.OmegaType
import com.github.detentor.omega.frontend.parser.ast.OmegaVariable
import com.github.detentor.omega.frontend.parser.ast.OmegaMethod
import com.github.detentor.omega.frontend.parser.ast.OmegaClass
import com.github.detentor.omega.backend.java.JavaConverter
import com.github.detentor.omega.frontend.parser.ast.OmegaStatement
import com.github.detentor.omega.frontend.parser.ast.Const
import com.github.detentor.omega.frontend.parser.ast.OmegaStaticMethodCallStatement
import com.github.detentor.omega.frontend.parser.ast.OmegaConstStatement
import scala.util.parsing.combinator.PackratParsers

/**
 * A princípio guardará a gramática da linguagem Omega.
 */
class NewOmegaGrammar extends RegexParsers with PackratParsers 
{
	def openParens : Parser[String] = "\\(".r
	def closeParens : Parser[String] = "\\)".r
	def openBracket = "\\{".r
	def closeBracket = "\\}".r
	def dotSymbol = "\\.".r ^^ { k => k}
	def argSeparator : Parser[String] = ",".r
	
	//keywords:
	def importKeyword = "import".r
	
	//Bloco Import
	def importBlock = importKeyword~openBracket~closeBracket
	
	//Declaração de método
	//tipo nomeMetodo(Int a, Int b) { }
	def identifier = "[a-zA-Z][a-zA-Z0-9]*".r ^^ { k => k}
	def classIdentifier = "[A-Z][a-zA-Z0-9]*".r ^^ { k => k}
	
	//definição de constantes
	def numLiteral = "[0-9]+".r ^^ { k => NumConst(k) }
	def stringLiteral = "\".*?\"".r ^^ { k => StringConst(k) }
	def const : Parser[Const] = numLiteral | stringLiteral

	
	//Tipo de uma classe: o tipo é um caminho completo, ou só o alias de um import
	lazy val classType : PackratParser[OmegaType] = ((identifier~dotSymbol~classType ^^ { case ident~dot~cType => OmegaType(ident + dot + cType.name) }) 
													|  (classIdentifier ^^ { cName => OmegaType(cName)}))  
	
	def methodArg : Parser[List[OmegaVariable]] = classType~identifier~(opt(argSeparator~methodArg)) ^^ 
	{
		case tipo~nome~None => List(OmegaVariable(tipo, nome))
		case tipo~nome~Some(sep~anotherArg) => List(OmegaVariable(tipo, nome)) ::: anotherArg
	}
	
	//definição de método
	def method = classType~identifier~openParens~opt(methodArg)~closeParens~openBracket~opt(rep(statement))~closeBracket ^^ 
	{
		case retType~mName~_~args~_~_~stats~_ => OmegaMethod(retType, mName, args.getOrElse(Nil), stats.getOrElse(Nil))
	}
	
	//definição de uma classe
	def classDeclaration : Parser[OmegaClass] = identifier~openBracket~importBlock~opt(rep(method))~closeBracket ^^ 
	{
		case nomeClasse~_~_~methods~_ => OmegaClass(nomeClasse, "" , methods.getOrElse(Nil))
	}
	
	//chamada de método estático
	def statement : Parser[OmegaStatement] = (const | staticCall) ^^ 
	{
		case constante : Const => OmegaConstStatement(constante)
		case sCall : OmegaStaticMethodCallStatement => sCall
		case _ => throw new IllegalArgumentException
	}
	
	def callArg = statement~opt(rep(argSeparator~>statement)) ^^ 
	{
		case firstStatement~others => List(firstStatement) ::: others.getOrElse(Nil)
	}
	
	lazy val staticCall : PackratParser[OmegaStaticMethodCallStatement] = classType~dotSymbol~identifier~openParens~opt(callArg)~closeParens ^^ {
		case fromClass~_~methodName~_~args~_ => OmegaStaticMethodCallStatement(fromClass, methodName, args.getOrElse(Nil)) 
	}
}

object NewOmegaGrammar
{
	def main(args: Array[String]) 
	{
		val parser = new NewOmegaGrammar
		
//		val cName = parser.parseAll(parser.classType, "java.com.Teste");
//		println(cName)
		
//		val mTry = parser.parseAll(parser.staticCall, "Integer.valueOf(\"55\")");
//		println(mTry)
		
		val resp = parser.parseAll(parser.classDeclaration, "Teste" +
				"{" +
				"import {}" + 
				"	Int sum(Int a, String b){" +
						"\"22\"" +
						"Integer.valueOf(\"55\")" +
					"\n}" +
				"}")
				
		println(new JavaConverter().convert(resp.get))
		
		println("\n\n" + resp)
  
	}
}