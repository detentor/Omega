package com.github.detentor.omega.frontend.parser.ast

//Representa um tipo em Omega. Um tipo pode ser uma referência a um import ou um fullPath
sealed class OmegaType protected(name : String) 
{
	override def toString = name
}

case class RelativeType(name : String) extends OmegaType(name)

case class AbsoluteType(name : String) extends OmegaType(name)
{
    def getRelativeName = name.substring(Math.max(0, name.lastIndexOf('.') + 1))
}