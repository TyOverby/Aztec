package com.prealpha.aztec


object Generator{
    val BULLET  = Some("*")
    val QUOTE   = Some("\"")
    val COMMENT = Some("#")
    val TITLE   = None
}

trait Generator {
    val shortName: String

    def genStart(symbol: Option[String]): String
    def gen(token: Token): String
    def genEnd(symbol: Option[String]): String

    def documentStart(document: List[String]): List[String]
    def documentEnd(document: List[String]): List[String]

    def postProcess(input: List[String]): List[String]
}
