package com.github.detentor.omega.frontend.parser.ast

/**
 * Um statement é uma expressão que retorna um tipo
 */
sealed trait Statement
{
   // def getReturnType : OmegaType
}

//case class StaticMethodCallStatement (ofClass : OmegaType, 
//								  methodName : String, 
//								  args : List[Statement] 
//								  //retType : OmegaType removido por simplificação
//								  ) 
//						extends Statement//(retType) 
//{
//
//}

case class ConstStatement(value : Const) extends Statement
{
    val getReturnType = value.getType
    override def toString = value.toString()
}

case class MethodCall(identifier : String, methodName : String) extends Statement
{
    override def toString = identifier + "." + methodName
}

case class VariableDeclaration(variable : OmegaVariable) extends Statement
{
    override def toString = variable.toString
}