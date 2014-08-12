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
import com.github.detentor.omega.frontend.parser.ast.MethodCall
import com.github.detentor.omega.frontend.parser.ast.OmegaMethod
import com.github.detentor.omega.frontend.parser.ast.VariableDeclaration
import com.github.detentor.omega.frontend.parser.ast.VariableDeclaration


//observações:

//1) a inferência de tipos significa que só é possível saber o tipo das expressões quando for a verificação
//semântica. Por isso o ideal é criar tipos bem leves, de forma que essa fase seja o menos custosa possível 
//do ponto de vista de performance.

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
	
  //definição de constantes (a refinar)
	def numLiteral = "[0-9]+".r ^^ { numVal => NumConst(numVal) }
	def stringLiteral = "\".*?\"".r ^^ { stringVal => StringConst(stringVal) }
	def constValue : Parser[Const] = numLiteral | stringLiteral

	//Identificadores
	def identifier = "[a-zA-Z][a-zA-Z0-9_]*".r ^^ { k => k}    //identificadores podem começar de qualquer jeito
	def varIdentifier = "[a-z][a-zA-Z0-9_]*".r ^^ { k => k}    //variáveis só podem começar com minúsculas
	def methodIdentifier = varIdentifier                       //Métodos só podem começar com minúsculas
	def classIdentifier = "[A-Z][a-zA-Z0-9_]*".r ^^ { k => k}  //classes só podem começar com maiúsculas
	
	//Bloco Import
	def importBlock = importKeyword~openBracket~rep(importDeclaration)~closeBracket ^^ { case _~_~imports~_ => imports }
	
	//Declaração de pacote
	def packageComb = dotSymbol~varIdentifier ^^ { case dot~varIdent => dot + varIdent  }
	def packageDeclaration = packageKeyword~(varIdentifier~rep(packageComb)) ^^ 
	{ 
	    case _~packName => AbsoluteType(packName._1 + packName._2.mkString)
	}

	def importComb = varIdentifier~dotSymbol ^^ { case ident~dot => ident + dot }
	def importDeclaration = rep(importComb)~classIdentifier ^^ { case ident~rest => AbsoluteType(ident.mkString + rest) }

	//Declaração de tipos (tipos relativos ao import e tipos absolutos)
	def typeDeclaration : Parser[OmegaType] = (classIdentifier | importDeclaration) ^^ 
	{
	     case classIdentif : String => RelativeType(classIdentif)
	     case importDecl : AbsoluteType  => importDecl
	}

	//Declaração de variáveis (o tipo é opcional)
	def varDeclaration = opt(mutKeyword)~opt(typeDeclaration)~varIdentifier~assignmentOperator~statement  ^^ 
	{
	     case isMutable~varType~varName~_~statement => VariableDeclaration(OmegaVariable(varType, varName, isMutable.isEmpty, statement))
	}
	
	//Declaração de métodos
	def methodDeclaration = typeDeclaration~methodIdentifier~openParens~closeParens~openBracket~rep(statement)~closeBracket ^^ 
	{
	    case retType~methodName~_~_~_~methodBody~_ => OmegaMethod(retType, methodName, Nil, methodBody)
	}

  //Statements
	def constStatement = constValue ^^ { constValue => ConstStatement(constValue) }
	def statement : Parser[Statement] = constStatement | instanceMethodCall | varDeclaration //Por enquanto só tem esse
	
	//chamada de método de instância
	def instanceMethodCall = varIdentifier~dotSymbol~methodIdentifier~openParens~closeParens ^^ 
	{
	    case varName~_~methodName~_~_ => MethodCall(varName, methodName)
	}
	
	
	
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
	  	  packageDeclaration~importBlock~classKeyword~identifier~openBracket~rep(varDeclaration)~rep(methodDeclaration)~closeBracket ^^ 
	{
	  case packageName~imports~_~nomeClasse~_~vars~methods~_ => OmegaClass(nomeClasse, packageName, vars.map(_.variable), methods)
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
		
		val source = """
        package legal.teste
        import 
        {
        }

        class Teste
        {
            valor = 0
            mut varTeste = valor.getValor()
            
            Int meuMetodo()
            {
                Int methodVar = 1
                mut methodVar2 = 0
            }
        }
      """
		
		val resp = parser.parseAll(parser.classDeclaration, source)
				
		println(new JavaConverter().convert(resp.get))
	}
}