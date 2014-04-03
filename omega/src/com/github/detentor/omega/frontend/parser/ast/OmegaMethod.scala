package com.github.detentor.omega.frontend.parser.ast

case class OmegaMethod(retType : OmegaType, 
					   name : String, 
					   params : List[OmegaVariable], 
					   methodBody : List[OmegaStatement] ) {

	override def toString = retType + " " + name + params.mkString("(", ", ", ")")
	
}