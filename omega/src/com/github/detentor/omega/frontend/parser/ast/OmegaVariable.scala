package com.github.detentor.omega.frontend.parser.ast

//Representa uma variável ou um field de uma classe
case class OmegaVariable(varType : OmegaType, name : String) 
{
	override def toString = varType + " " + name
	def toJava = varType + " " + name
}