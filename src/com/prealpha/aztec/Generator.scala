package com.prealpha.aztec

trait Generator {
    val shortName: String

    def genStart(symbol: Token): String
    def gen(token: Token): String
    def genEnd(symbol: Token): String

    def documentStart(document: List[Token]): List[String]
    def documentEnd(document: List[Token]): List[String]

    def postProcess(input: List[String]): List[String]
}

object Generator{
    val BULLET  = Some("*")
    val DOLLAR  = Some("$")
    val QUOTE   = Some("\"")
    val COMMENT = Some("#")
    val TITLE   = None

    def removeBlank(token: Token)(f: => String): String = {
        token match {
            case _ : EmptyWithIndent => ""
            case _ : EmptyLine => ""
            case _ => f
        }
    }
}


