package com.prealpha.aztec


trait Generator {
    val shortName: String

    def genStart(symbol: Token): Option[String]
    def gen(token: Token): Option[String]
    def genEnd(symbol: Token): Option[String]

    def documentStart(document: List[Token]): List[String]
    def documentEnd(document: List[Token]): List[String]

    def postProcess(input: List[String]): List[String]
}

object Generator{
    val SETTING = Some("{")
    val CODE    = Some("~")
    val MONO    = Some("`")
    val BULLET  = Some("*")
    val DOLLAR  = Some("$")
    val QUOTE   = Some("\"")
    val COMMENT = Some("#")
    val TITLE   = None
    // Because the lexer takes the characters and numbers
    // off the front, we can just match on these.
    val NUMERIC = Some(".")
    val ALPHA   = Some(")")

    def removeBlank(token: Token)(f: => Option[String]): Option[String] = {
        token match {
            case _ : EmptyWithIndent => Some("")
            case _ : EmptyLine => Some("")
            case _ => f
        }
    }
}


