package com.prealpha.aztec.beta.generators

import com.prealpha.aztec.{Generator, Token}
import Generator._

object Latex extends Generator{
    val shortName: String = "latex"

    var lineNumber = 0

    def genStart(token: Token): Option[String] = removeBlank(token){
        token.symbol match {
            case QUOTE   => None
            case COMMENT => None
            case TITLE   => None
            case CODE    => Some("\\begin{verbatim}")
            case MONO    => Some("\\begin{verbatim}")
            case DOLLAR  => Some("\\begin{align}")
            case BULLET  => Some("\\begin{itemize}")
            case NUMERIC => Some("\\begin{enumerate}[1.]")
            case ALPHA   => Some("\\begin{enumerate}[a)]")

            case Some(x) => x match{
                case _   => Some("% UNKNOWN START SYMBOL: " + x)
            }
        }
    }

    def gen(token: Token): Option[String] = removeBlank(token) {
        val content = token.content.getOrElse("")
        Some(token.symbol match{
            case QUOTE   => content
            case COMMENT => "% " + content
            case DOLLAR  => content + " \\\\"
            case BULLET  => "\\item " + content
            case NUMERIC => "\\item " + content
            case ALPHA   => "\\item " + content
            case TITLE   => "\\section{"+content+"}"
            case CODE    => {
                lineNumber += 1
                val num = lineNumber.toString
                val extraSpaces = " " * (3-num.length+token.innerIndent)
                num + extraSpaces + content
            }
            case MONO => content

            case Some(x) => x match{
                case _   => "% UNKNOWN GEN SYMBOL: " + x
            }
        })
    }

    def genEnd(token: Token): Option[String] = removeBlank(token){
        token.symbol match{
            case COMMENT => None
            case TITLE   => None
            case QUOTE   => Some("\n")
            case DOLLAR  => Some("\\end{align}")
            case BULLET  => Some("\\end{itemize}")
            case NUMERIC => Some("\\end{enumerate}")
            case ALPHA   => Some("\\end{enumerate}")
            case CODE    => lineNumber = 0; Some("\\end{verbatim}")
            case MONO    => Some("\\end{verbatim}")

            case Some(x) => x match{
                case _       => Some("% UNKNOWN END SYMBOL: " + x)
            }
        }
    }

    def documentStart(document: List[Token]): List[String] ={
        val settings = document.filter(_.symbol == Some("{"))
        val pairings: List[(String, String)] = settings.flatMap(_.content)
            .map(_.split("="))               // Now we have an array of strings
            .filter(_.length==2)             // We only want the ones that are a pair
            .map(a=> (a(0).trim, a(1).trim)) // Build a tuple out of them
        val settingsMap = Map(pairings:_*)   // Add them to the map

        def getSetting(key: String) = settingsMap.get(key).getOrElse(key)

        val name  = getSetting("name")
        val title = getSetting("title")
        val date  = getSetting("date")

        val standard = List(
            "\\documentclass[12pt]{article}",
            "\\parindent=0pt",
            "\\usepackage{amsmath}",
            "\\usepackage{enumerate}",
            "\\usepackage[margin=0.5in]{geometry}",
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
