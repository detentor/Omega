package com.github.detentor.omega.frontend.parser.ast

/**
 * Um statement é uma expressão que retorna um tipo
 */
sealed class Statement protected(val retType : OmegaType) 
{

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

case class ConstStatement(value : Const) extends Statement(value.getType)
{
    override def toString = value.toString()
}