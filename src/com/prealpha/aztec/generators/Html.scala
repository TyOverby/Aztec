package com.prealpha.aztec.beta.generators

import com.prealpha.aztec.{Generator, Token}
import Generator._
import com.prealpha.aztec.{Generator, Token}

object Html extends Generator{
    val shortName: String = "html"


    def genStart(token: Token): Option[String] = removeBlank(token){
        Some(token.symbol match {
            case BULLET  => "<ul>"
            case QUOTE   => "<p>"
            case COMMENT => "<!--"
            case TITLE   => "<div class='title'>"
            case _       => "<!-- UNKNOWN START SYMBOL: " + token.symbol.get
        })
    }

    def gen(token: Token): Option[String] = {
        val content = token.content.getOrElse("")
        Some(token.symbol match {
            case BULLET  => "<li>" + content + "</li>"
            case QUOTE   => content
            case COMMENT => content
            case TITLE   => "<h1>" + content + "</h1>"
            case _       => "UNKNOWN GEN SYMBOL: " + token.symbol
        })
    }

    def genEnd(token: Token): Option[String] = removeBlank(token){
        Some(token.symbol match {
            case BULLET  => "</ul>"
            case QUOTE   => "</p>"
            case COMMENT => "-->"
            case TITLE   => "</div>"
            case _       => "UNKNOWN END SYMBOL: " + token.symbol + " -->"
        })
    }

    def postProcess(input: List[String]): List[String] = input

    def documentStart(document: List[Token]): List[String] = List("<html>", "<body>")
    def documentEnd(document: List[Token]): List[String] = List("</body>", "</html>")
}
