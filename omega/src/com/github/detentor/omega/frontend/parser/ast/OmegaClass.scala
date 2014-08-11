package com.github.detentor.omega.frontend.parser.ast

case class OmegaClass(name : String, thePackage : AbsoluteType, fields : List[OmegaVariable], methods : List[OmegaMethod]) 
{
  
   override def toString = 
   {
        ""
   }
   
   def toJava = 
   {
       "package " + thePackage + "\n" + 
       //imports line
       "public " + name + "{" + "\n" + 
       fields.map(_.toJava).mkString("\n") + 
       "}"
   }
}