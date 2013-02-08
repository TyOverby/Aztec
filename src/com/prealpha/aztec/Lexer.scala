package com.prealpha.aztec

import io.Source


object Lexer {
    private[this]
    def countLeading(in: String): Int = {
        in.takeWhile{
            case ' '  => true
            case '\t' => true
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
            if (line.trim.endsWith("\\") && !line.trim.endsWith("\\\\")){
                val nextLine = lexLine(lineReader.next())._1.trim
                (line.reverse.replaceFirst("\\\\","").reverse +
                  (if (nextLine.startsWith("\\")) nextLine.substring(1) else nextLine), number)
            }
            else{
                (line, number)
            }
        }

        def genToken(lineWithNumber: (String, Int)): Token = {
            val (line, number) = lineWithNumber

            if (line.trim.length == 0) return EmptyLine(number)

            val indent = countLeading(line)
            val broken = line.trim.split(" ").flatMap(_.split("\t"))
            val first = broken.head
            val (symbol, rest) = if (isSymbolic(first))
                                    (Some(first.dropWhile(x=> x.isDigit||x.isLetter)), broken.tail.mkString(" "))
                                    else (None, broken.mkString(" "))

            val innerIndent = countLeading(line.substring(indent+symbol.getOrElse("").size))

            VerboseToken(symbol,rest,indent, innerIndent,number)
        }

        lineReader.map(lexLine).map(genToken).toList
    }
    def lex(input: String):List[Token] = lex(Source.fromString(input))
}
