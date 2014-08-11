package com.github.detentor.omega.frontend.parser

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers
import com.github.detentor.omega.frontend.parser.ast.AbsoluteType
import com.github.detentor.omega.frontend.parser.ast.AbsoluteType
import com.github.detentor.omega.frontend.parser.ast.Const
import com.github.detentor.omega.frontend.parser.ast.ConstStatement
import com.github.detentor.omega.frontend.parser.ast.ConstStatement
import com.github.detentor.omega.frontend.parser.ast.NumConst
import com.github.detentor.omega.frontend.parser.ast.NumConst
import com.github.detentor.omega.frontend.parser.ast.OmegaClass
import com.github.detentor.omega.frontend.parser.ast.OmegaType
import com.github.detentor.omega.frontend.parser.ast.OmegaVariable
import com.github.detentor.omega.frontend.parser.ast.RelativeType
import com.github.detentor.omega.frontend.parser.ast.StringConst
import com.github.detentor.omega.frontend.parser.ast.StringConst
import com.github.detentor.omega.frontend.parser.ast.Statement
import com.github.detentor.omega.backend.java.JavaConverter

//A princípio guardará a gramática da linguagem Omega.
class NewOmegaGrammar extends RegexParsers with PackratParsers 
{
  
  //Outros
	def openParens : Parser[String] = "\\(".r
	def closeParens : Parser[String] = "\\)".r
	def openBracket = "\\{".r 
	def closeBracket = "\\}".r
	def dotSymbol = "\\.".r 
	def argSeparator : Parser[String] = ",".r
	def assignmentOperator = "=".r

	//keywords:
	def classKeyword = "class".r
	def importKeyword = "import".r
	def packageKeyword = "package".r
	def mutKeyword = "mut".r
	
  //definição de constantes
	def numLiteral = "[0-9]+".r ^^ { numVal => NumConst(numVal) }
	def stringLiteral = "\".*?\"".r ^^ { stringVal => StringConst(stringVal) }
	def constValue : Parser[Const] = numLiteral | stringLiteral

	//Identificadores
	def identifier = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ { k => k}    //identificadores podem começar de qualquer jeito
	def varIdentifier = "[a-z][a-zA-Z0-9_]*".r ^^ { k => k}    //variáveis só podem começar com minúsculas
	def classIdentifier = "[A-Z][a-zA-Z0-9_]*".r ^^ { k => k}  //classes só podem começar com maiúsculas
	
	//Bloco Import
	def importBlock = importKeyword~openBracket~rep(importDeclaration)~closeBracket ^^ { case _~_~imports~_ => imports }
	
	//Declaração de pacote
	def packageDeclaration = packageKeyword~importDeclaration ^^ { case _~packageName => packageName }
	def importComb = dotSymbol~identifier ^^ { case dot~ident => dot + ident }
	def importDeclaration = identifier~rep(importComb) ^^ { case ident~rest => AbsoluteType(ident + rest.mkString("")) }
	
	//Depois alterar para refletir os dois tipos possíveis
	def typeDeclaration : Parser[OmegaType] = (importDeclaration | classIdentifier) ^^ 
	{
	     case classIdentif : String => RelativeType(classIdentif)
	     case importDecl : AbsoluteType  => importDecl
	}
	
	//Declaração de variáveis: versão full (com o tipo) e versão curta (sem o tipo)
	def varDeclFull = typeDeclaration~varIdentifier~assignmentOperator~statement ^^ { case varType~varName~_~varValue => Tuple3(varType, varName, varValue)  }
	def varDeclShort = varIdentifier~assignmentOperator~statement ^^ { case varName~_~varValue => Tuple3(varValue.retType, varName, varValue)  }

	def varDeclaration = opt(mutKeyword)~(varDeclShort | varDeclFull)  ^^ 
	{
	     case isMutable~omegaVar => OmegaVariable(omegaVar._1, omegaVar._2, isMutable.isEmpty, omegaVar._3)
	}
	
	//Statement
	def constStatement = constValue ^^ { constValue => ConstStatement(constValue) }
	
	def statement : Parser[Statement] = constStatement //Por enquanto só tem esse
	
	//	//chamada de método estático
//	def statement : Parser[OmegaStatement] = (const | staticCall) ^^ 
//	{
//		case constante : Const => OmegaConstStatement(constante)
//		case sCall : OmegaStaticMethodCallStatement => sCall
//		case _ => throw new IllegalArgumentException
//	}
	
	
	//Declaração de método
	//tipo nomeMetodo(Int a, Int b) { }

	


	
	//Tipo de uma classe: o tipo é um caminho completo, ou só o alias de um import
//	lazy val classType : PackratParser[OmegaType] = ((identifier~dotSymbol~classType ^^ { case ident~dot~cType => OmegaType(ident + dot + cType.name) }) 
//													|  (classIdentifier ^^ { cName => OmegaType(cName)}))  
	
//	def methodArg : Parser[List[OmegaVariable]] = classType~identifier~(opt(argSeparator~methodArg)) ^^ 
//	{
//		case tipo~nome~None => List(OmegaVariable(tipo, nome))
//		case tipo~nome~Some(sep~anotherArg) => List(OmegaVariable(tipo, nome)) ::: anotherArg
//	}
	
	//definição de método
//	def method = classType~identifier~openParens~opt(methodArg)~closeParens~openBracket~opt(rep(statement))~closeBracket ^^ 
//	{
//		case retType~mName~_~args~_~_~stats~_ => OmegaMethod(retType, mName, args.getOrElse(Nil), stats.getOrElse(Nil))
//	}
	
	
	def classDeclaration : Parser[OmegaClass] = 
	  	  packageDeclaration~importBlock~classKeyword~identifier~openBracket~rep(varDeclaration)~closeBracket ^^ 
	{
	  case packageName~imports~_~nomeClasse~_~vars~_ => OmegaClass(nomeClasse, packageName, vars, Nil)
		//case packageName~imports~nomeClasse~_~_~_ => )
	}
	
//	//chamada de método estático
//	def statement : Parser[OmegaStatement] = (const | staticCall) ^^ 
//	{
//		case constante : Const => OmegaConstStatement(constante)
//		case sCall : OmegaStaticMethodCallStatement => sCall
//		case _ => throw new IllegalArgumentException
//	}
	
//	def callArg = statement~opt(rep(argSeparator~>statement)) ^^ 
//	{
//		case firstStatement~others => List(firstStatement) ::: others.getOrElse(Nil)
//	}
//	
//	lazy val staticCall : PackratParser[OmegaStaticMethodCallStatement] = classType~dotSymbol~identifier~openParens~opt(callArg)~closeParens ^^ {
//		case fromClass~_~methodName~_~args~_ => OmegaStaticMethodCallStatement(fromClass, methodName, args.getOrElse(Nil)) 
//	}
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
		
		val resp = parser.parseAll(parser.classDeclaration,
		    "package legal.Teste " + "\n" +
		    "import {}" + "\n" +
		    "class Teste" +
				"{" + "\n" +
				 "valor = 0" + "\n" + 
				"}")
				
		println(new JavaConverter().convert(resp.get))
	}
}