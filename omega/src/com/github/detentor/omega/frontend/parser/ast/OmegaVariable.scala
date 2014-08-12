package com.github.detentor.omega.frontend.parser.ast

//Representa uma variável ou um field de uma classe
case class OmegaVariable(varType : Option[OmegaType], name : String, isFinal : Boolean, initValue : Option[Statement]) 
{
  //O tipo da variável está como option porque ele pode ser inferido
	override def toString = varType.getOrElse("-") + " " + name +  "  " + initValue.getOrElse("")
}