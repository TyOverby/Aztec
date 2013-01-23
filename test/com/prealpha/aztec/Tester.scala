package com.prealpha.aztec

import com.prealpha.aztec.{Lexer, Parser}
import scala.io.Source

object Tester extends App{
    val lexed =  Lexer.lex(Source.fromFile("testing.aztec"))
    val parsed = Parser.parse(lexed)


    lexed.foreach(println)
    println()
    println()
    parsed.print()

    val parsedInserted = Parser.insertParents(parsed)
    println(); println()
    parsedInserted.print()

}
