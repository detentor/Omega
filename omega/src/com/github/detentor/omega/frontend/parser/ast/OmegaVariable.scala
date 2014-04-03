package com.github.detentor.omega.frontend.parser.ast

case class OmegaVariable(varType : OmegaType, name : String) 
{
	override def toString = varType + " " + name
}