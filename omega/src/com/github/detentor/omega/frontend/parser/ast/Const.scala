package com.github.detentor.omega.frontend.parser.ast

/**
 * ADT que representa as constantes da linguagem
 */
sealed trait Const 
{
	def getValue() : String
	def getType : OmegaType
}

sealed case class StringConst(val theValue : String) extends Const
{
	def getValue = theValue
	def getType = AbsoluteType("java.util.String")
	override def toString = theValue
}

sealed case class NumConst(val theValue : String) extends Const
{
	def getValue = theValue
	def getType = AbsoluteType("java.util.Integer") //TODO: alterar depois
	override def toString = theValue
}