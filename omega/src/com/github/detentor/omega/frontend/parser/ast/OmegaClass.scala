package com.github.detentor.omega.frontend.parser.ast

case class OmegaClass(name : String, thePackage : AbsoluteType, fields : List[OmegaVariable], methods : List[OmegaMethod]) 
{
}