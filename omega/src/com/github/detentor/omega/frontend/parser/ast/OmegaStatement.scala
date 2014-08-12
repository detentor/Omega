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

case class MethodCall(identifier : String, methodName : String, args : List[Statement]) extends Statement
{
    override def toString = identifier + "." + methodName
}

case class StaticMethodCall(fromType : OmegaType, methodName : String, args : List[Statement]) extends Statement
{
    override def toString = fromType + "." + methodName
}

case class VariableDeclaration(variable : OmegaVariable) extends Statement
{
    override def toString = variable.toString
}