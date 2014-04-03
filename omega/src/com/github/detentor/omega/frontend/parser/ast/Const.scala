package com.github.detentor.omega.frontend.parser.ast

/**
 * ADT que representa as constantes da linguagem
 */
sealed trait Const 
{
	def getValue() : String
}

sealed case class StringConst(val theValue : String) extends Const
{
	def getValue = theValue
}

sealed case class NumConst(val theValue : String) extends Const
{
	def getValue = theValue
}