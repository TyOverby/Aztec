package com.prealpha.aztec

import io.Source


object Lexer {
    private[this]
    def countLeading(in: String):Int = {
        in.takeWhile{
            case ' '  => true
            case '\n' => true
            case _ => false
        }.size
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

            if (line.trim.length==0) return EmptyLine(number)

            val indents = countLeading(line)
            val indents_removed = line.substring(indents)

            val symbolEnd = indents_removed.indexWhere{
                case ' '  => true
                case '\t' => true
                case _    => false
            }
            val symbol = indents_removed.substring(0,symbolEnd)
            val isSymbolic = symbol.exists{ case x => !(x.isLetter || x.isDigit) }

            val rest = indents_removed.substring(if (isSymbolic) symbolEnd else 0)

            VerboseToken((if (isSymbolic) Some(symbol) else None), rest, indents, number)
        }

        lineReader.map(lexLine).map(genToken).toList
    }
    def lex(input: String):List[Token] = lex(Source.fromString(input))
}
