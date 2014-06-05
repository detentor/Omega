package com.github.detentor.omega.backend.java

import com.github.detentor.omega.frontend.parser.ast.OmegaClass
import com.github.detentor.omega.frontend.parser.ast.OmegaMethod
import com.github.detentor.omega.frontend.parser.ast.OmegaVariable
import com.github.detentor.omega.frontend.parser.ast.OmegaStatement
import com.github.detentor.omega.frontend.parser.ast.Const
import com.github.detentor.omega.frontend.parser.ast.OmegaStaticMethodCallStatement
import com.github.detentor.omega.frontend.parser.ast.OmegaConstStatement

/**
 * Responsável por transformar código da linguagem Omega para a linguagem Java
 */
class JavaConverter 
{
	def convert(theClass : OmegaClass) : String = 
	{
		"public class " + theClass.name + "\n{\n" + theClass.methods.map(convert).mkString("\n") + "}\n"
	}
	
	def convert(theMethod : OmegaMethod) : String = 
	{
		"public " + theMethod.retType + " " + theMethod.name + 
			theMethod.params.map(convert).mkString("(", ", ", ")") + 
		"\n{\n" + 
			theMethod.methodBody.map(convert).mkString(";\n") + 
			";\n}\n"
	}
	
	def convert(theVariable : OmegaVariable) : String = 
	{
		theVariable.varType + " " + theVariable.name
	}
	
	def convert(theStatement : OmegaStatement) : String = 
	{
		theStatement match
		{
			case someConst : OmegaConstStatement => someConst.value.getValue
			case staticCall : OmegaStaticMethodCallStatement => staticCall.ofClass + "." + staticCall.methodName + 
																	staticCall.args.map(convert).mkString("(", ", ", ")")
		}
	}

}