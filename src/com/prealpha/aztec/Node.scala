package com.prealpha.aztec

sealed trait Tree{
    val token: Token
    val children: List[Tree]

    def print(indent:Int = 0){
        println("  " * indent + token.toString)
        children.foreach(_.print(indent+1))
    }
}

case class Node(token: Token, children: List[Tree]) extends Tree

case class Root(children: List[Tree]) extends Tree{
    val token = NonToken
}