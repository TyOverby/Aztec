package com.prealpha.aztec.beta.generators

import com.prealpha.aztec.{Generator, Token}
import Generator._
import com.prealpha.aztec.{Generator, Token}

object Html extends Generator{
    val shortName: String = "html"


    def genStart(symbol: Option[String]): String = symbol match {
        case BULLET  => "<ul>"
        case QUOTE   => "<p>"
        case COMMENT => "<!--"
        case TITLE   => "<div class='title'>"
        case _       => "<!-- UNKNOWN START SYMBOL: " + symbol.get + "-->"
    }

    def gen(token: Token): String ={
        val content = token.content.getOrElse("")
        token.symbol match {
            case BULLET  => "<li>" + content + "</li>"
            case QUOTE   => content
            case COMMENT => content
            case TITLE   => "<h1>" + content + "</h1>"
            case _       => "UNKNOWN GEN SYMBOL: " + token.symbol
        }
    }

    def genEnd(symbol: Option[String]): String = symbol match {
        case BULLET  => "</ul>"
        case QUOTE   => "</p>"
        case COMMENT => "-->"
        case TITLE   => "</div>"
        case _       => "UNKNOWN END SYMBOL: " + symbol
    }

    def postProcess(input: List[String]): List[String] = input

    def documentStart(document: List[String]): List[String] = List("<html>", "<body>")
    def documentEnd(document: List[String]): List[String] = List("</body>", "</html>")
}
