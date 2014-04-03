package com.github.detentor.omega.frontend.parser.ast

/**
 * Um statement é uma expressão que retorna um tipo
 */
trait OmegaStatement//(retType : OmegaType) 
{

}

case class OmegaStaticMethodCallStatement (ofClass : OmegaType, 
								  methodName : String, 
								  args : List[OmegaStatement] 
								  //retType : OmegaType removido por simplificação
								  ) 
						extends OmegaStatement//(retType) 
{

}

case class OmegaConstStatement(value : Const) extends OmegaStatement