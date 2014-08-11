package com.github.detentor.omega.backend.java

import com.github.detentor.omega.frontend.parser.ast.OmegaClass
import com.github.detentor.omega.frontend.parser.ast.OmegaVariable

/**
 * Responsável por transformar código da linguagem Omega para a linguagem Java
 */
class ByteCodeConverter 
{
	def convert(theClass : OmegaClass) : String = 
	{
	  "package " + theClass.thePackage + ";\n" + 
    //imports line
	  "public " + theClass.name + "{" + "\n" + 
    theClass.fields.map("private " + convert(_)).mkString("\n") + 
    "\n}"
	}
	
//	def convert(theMethod : OmegaMethod) : String = 
//	{
//		"public " + theMethod.retType + " " + theMethod.name + 
//			theMethod.params.map(convert).mkString("(", ", ", ")") + 
//		"\n{\n" + 
//			theMethod.methodBody.map(convert).mkString(";\n") + 
//			";\n}\n"
//	}
//	
	def convert(theVariable : OmegaVariable) : String = 
	{
		  (if (theVariable.isFinal) "final " else "") + theVariable.varType + " " + theVariable.name + " = " + theVariable.initValue + ";"
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