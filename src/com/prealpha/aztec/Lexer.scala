package com.prealpha.aztec

import io.Source


object Lexer {
    private[this]
    def countLeading(in: String): Int = {
        in.takeWhile{
            case ' '  => true
            case '\n' => true
            case _ => false
        }.size
    }
    def isSymbolic(in: String): Boolean =  in.exists {
            case x => !(x.isDigit || x.isLetter)
    }

    def lex(input: Source):List[Token] = {
        val lineReader = input.getLines().zipWithIndex

        def lexLine(lineWithNumber: (String, Int)): (String, Int) = {
            val (line, number) = lineWithNumber
            if (line.trim.endsWith("\\")){

                (line.reverse.replaceFirst("\\\\","").reverse + lexLine(lineReader.next())._1.trim, number)
            }
            else{
                (line, number)
            }
        }

        def genToken(lineWithNumber: (String, Int)): Token = {
            val (line, number) = lineWithNumber

            if (line.trim.length == 0) return EmptyLine(number)

            val indent = countLeading(line)
            val broken = line.trim.split(" ")
            val first = broken.head
            val (symbol, rest) = if (isSymbolic(first))
                                    (Some(first), broken.tail.mkString(" "))
                                    else (None, broken.mkString(" "))

            VerboseToken(symbol,rest,indent,number)
        }

        lineReader.map(lexLine).map(genToken).toList
    }
    def lex(input: String):List[Token] = lex(Source.fromString(input))
}
