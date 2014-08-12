package com.github.detentor.omega.backend.java

import com.github.detentor.omega.frontend.parser.ast.OmegaClass
import com.github.detentor.omega.frontend.parser.ast.OmegaVariable
import com.github.detentor.omega.frontend.parser.ast.OmegaMethod
import com.github.detentor.omega.frontend.parser.ast.Statement
import com.github.detentor.omega.frontend.parser.ast.VariableDeclaration
import com.github.detentor.omega.frontend.parser.ast.MethodCall
import com.github.detentor.omega.frontend.parser.ast.MethodCall
import com.github.detentor.omega.frontend.parser.ast.StaticMethodCall
import com.github.detentor.omega.frontend.parser.ast.StaticMethodCall
import com.github.detentor.omega.frontend.parser.ast.ConstStatement

/**
 * Responsável por transformar código da linguagem Omega para a linguagem Java
 */
class JavaConverter 
{
	def convert(theClass : OmegaClass) : String = 
	{
	  "package " + theClass.thePackage + ";\n" + 
    //imports line
	  "public " + theClass.name + "{" + "\n" + 
    theClass.fields.map("private " + convert(_)).mkString("\n") + "\n" +  
    theClass.methods.map("public " + convert(_)).mkString("\n") +
    "\n}"
	}
	
	def convert(theMethod : OmegaMethod) : String = 
	{
	    theMethod.toString + "\n{\n" + theMethod.methodBody.map("\t" + convert(_)).mkString("\n") + "\n}\n"
	}
	
	def convert(methodCall : MethodCall) : String = 
	{
	    methodCall.identifier + "." + methodCall.methodName + "(" + methodCall.args.map(convert).mkString(",") + ")"
	}
	
	def convert(methodCall : StaticMethodCall) : String = 
	{
	    methodCall.fromType + "." + methodCall.methodName + "(" + methodCall.args.map(convert).mkString(",") + ")"
	}
	
	def convert(theVariable : OmegaVariable) : String = 
	{
		  (if (theVariable.isFinal) "final " else "") + theVariable.varType.getOrElse("") + " " + theVariable.name + " = " + theVariable.initValue.map(convert).getOrElse("-") + ";"
	}
	
	
	def convert(theStatement : Statement) : String = 
	{
	    theStatement match
	    {
	      case constValue : ConstStatement => constValue.toString()
	      case varDeclaration : VariableDeclaration => convert(varDeclaration.variable)
	      case methodCall : MethodCall => convert(methodCall)
	      case staticMethodCall : StaticMethodCall => 
	        {
	           println("entrou aqui")
	           convert(staticMethodCall)   
	        }
	    }
	}
	
//	
//	def convert(theStatement : OmegaStatement) : String = 
//	{
//		theStatement match
//		{
//			case someConst : OmegaConstStatement => someConst.value.getValue
//			case staticCall : OmegaStaticMethodCallStatement => staticCall.ofClass + "." + staticCall.methodName + 
//																	staticCall.args.map(convert).mkString("(", ", ", ")")
//		}
//	}

}