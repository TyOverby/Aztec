package com.prealpha.aztec.beta.generators

import com.prealpha.aztec.{Generator, Token}
import Generator._

object Latex extends Generator{
    val shortName: String = "latex"

    def genStart(symbol: Option[String]): String = symbol match {
        case DOLLAR  => "\\begin{align}"
        case QUOTE   => "\n"
        case BULLET  => "\\begin{itemize}"
        case COMMENT => ""
        case TITLE   => ""
        case _       => "% UNKNOWN START SYMBOL: " + symbol.get
    }

    def gen(token: Token): String = {
        val content = token.content.getOrElse("")
        token.symbol match{
            case DOLLAR  => content + " \\\\"
            case QUOTE   => content
            case BULLET  => "\\item " + content + "\n"
            case COMMENT => "% " + content
            case TITLE   => "\\section{"+content+"}"
            case _       => "% UNKNOWN GEN SYMBOL: " + token.symbol
        }
    }

    def genEnd(symbol: Option[String]): String = symbol match{
        case DOLLAR  => "\\end{align}"
        case QUOTE   =>  "\n"
        case BULLET  =>  "\\end{itemize}"
        case COMMENT => ""
        case TITLE   => ""
        case _       => "% UNKNOWN END SYMBOL: " + symbol
    }

    def documentStart(document: List[String]): List[String] =
        """
          |\\documentclass[12pt]{article}
          |\\usepackage{amsmath}
          |\\title{\LaTeX}
          |\\date{}
          |\\begin{document}
          |\\maketitle
        """.stripMargin :: Nil

    def documentEnd(document: List[String]): List[String] = "\\end{document}" :: Nil

    def postProcess(input: List[String]): List[String] = input match {
        case Nil => Nil
        // The last line before an end align shouldn't have a line break
        case str :: "\\end{align}" :: xs => str.reverse.substring(2).reverse :: "\\end{align}" :: postProcess(xs)
        case x :: xs => x :: postProcess(xs)
    }
}
