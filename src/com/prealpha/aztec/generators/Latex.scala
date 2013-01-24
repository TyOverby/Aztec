package com.prealpha.aztec.beta.generators

import com.prealpha.aztec.{Generator, Token}
import Generator._

object Latex extends Generator{
    val shortName: String = "latex"

    def genStart(symbol: Option[String]): String = symbol match {
        case QUOTE   => "\n"
        case BULLET  => "\\begin{itemize}"
        case COMMENT => ""
        case TITLE   => ""
        case _       => "% UNKNOWN START SYMBOL: " + symbol.get
    }

    def gen(token: Token): String = {
        val content = token.content.getOrElse("")
        token.symbol match{
            case QUOTE   => content
            case BULLET  => "\\item " + content + "\n"
            case COMMENT => "% " + content
            case TITLE   => "\\section{"+content+"}"
            case _       => "% UNKNOWN GEN SYMBOL: " + token.symbol
        }
    }

    def genEnd(symbol: Option[String]): String = symbol match{
        case QUOTE   =>  "\n"
        case BULLET  =>  "\\end{itemize}"
        case COMMENT => ""
        case TITLE   => ""
        case _       => "% UNKNOWN END SYMBOL: " + symbol
    }

    def documentStart(document: List[String]): List[String] =
        """
          | \\documentclass[12pt]{article}
          |  \\usepackage{amsmath}
          |  \\title{\LaTeX}
          |  \\date{}
          |  \\begin{document}
          |    \\maketitle
        """.stripMargin :: Nil

    def documentEnd(document: List[String]): List[String] = "\\end{document}" :: Nil

    def postProcess(input: List[String]): List[String] = input
}
