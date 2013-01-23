package com.prealpha.aztec

sealed trait Token {
    val lineNumber: Int
    val symbol: Option[String]
    val content: Option[String]
    val indent: Int
}


case class VerboseToken(symbol: Option[String], con: String, indent: Int, lineNumber: Int) extends Token{
    val content = Some(con)
}
case class EmptyLine(lineNumber: Int) extends Token{
    val symbol:  Option[String] = None
    val content: Option[String] = None
    val indent = -1
}
case class EmptyWithIndent(lineNumber: Int, indent: Int) extends Token {
    val symbol:  Option[String] = None
    val content: Option[String] = None
}

case class ParentToken(symbol: Option[String], lineNumber: Int, indent: Int) extends Token{
    def this(other: Token) = this(other.symbol,other.lineNumber,other.indent)

    val content: Option[String] = None
}

case object NonToken extends Token{
    val lineNumber = -1
    val symbol = None
    val content = None
    val indent = -1
}