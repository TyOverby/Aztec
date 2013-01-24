package com.prealpha.aztec.beta.generators

import com.prealpha.aztec.{EmptyWithIndent, Generator, Token}
import Generator._

object Latex extends Generator{
    val shortName: String = "latex"



    def genStart(token: Token): String = removeBlank(token){
        token.symbol match {
            case DOLLAR  => "\\begin{align}"
            case QUOTE   => "\n"
            case BULLET  => "\\begin{itemize}"
            case COMMENT => ""
            case TITLE   => ""
            case _       => "% UNKNOWN START SYMBOL: " + token.symbol.get
        }
    }

    def gen(token: Token): String = removeBlank(token) {
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

    def genEnd(token: Token): String = removeBlank(token){
        token.symbol match{
            case DOLLAR  => "\\end{align}"
            case QUOTE   =>  "\n"
            case BULLET  =>  "\\end{itemize}"
            case COMMENT => ""
            case TITLE   => ""
            case _       => "% UNKNOWN END SYMBOL: " + token.symbol
        }
    }

    def documentStart(document: List[Token]): List[String] ={
        val settings = document.filter(_.symbol == Some("{"))
        val pairings: List[(String, String)] = settings.flatMap(_.content).map(_.split("=")).filter(_.length==2).map(a=>(a(0).trim,a(1).trim))
        val settingsMap = Map(pairings:_*)

        def getSetting(key: String) = settingsMap.get(key).getOrElse(key)

        val name  = getSetting("name")
        val title = getSetting("title")
        val date  = getSetting("date")

        val standard = List(
            "\\documentclass[12pt]{article}",
            "\\usepackage{amsmath}",
            "\\title{"+title+"\\\\"+name+"}",
            "\\date{"+date+"}",
            "\\begin{document}",
            "\\maketitle")
        standard
    }

    def documentEnd(document: List[Token]): List[String] = "\\end{document}" :: Nil

    def postProcess(input: List[String]): List[String] = input match {
        case Nil => Nil
        // The last line before an end align shouldn't have a line break
        case str :: "\\end{align}" :: xs => str.reverse.substring(2).reverse :: "\\end{align}" :: postProcess(xs)
        case x :: xs => x :: postProcess(xs)
    }
}
