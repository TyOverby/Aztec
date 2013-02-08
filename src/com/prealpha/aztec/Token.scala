package com.prealpha.aztec

sealed trait Token {
    val lineNumber: Int
    val symbol: Option[String]
    val content: Option[String]
    val indent: Int
    val innerIndent: Int
}


case class VerboseToken(symbol: Option[String], con: String, indent: Int, innerIndent: Int, lineNumber: Int) extends Token{
    val content = Some(con)
}

// TODO: Get rid of some of the empty line shit
case class EmptyLine(lineNumber: Int) extends Token{
    val symbol:  Option[String] = None
    val content: Option[String] = None
    val indent = -1
    val innerIndent = -1
}
case class EmptyWithIndent(lineNumber: Int, indent: Int) extends Token {
    val symbol:  Option[String] = None
    val content: Option[String] = None
    val innerIndent = -1
}

case class ParentToken(symbol: Option[String], lineNumber: Int, indent: Int) extends Token{
    def this(other: Token) = this(other.symbol,other.lineNumber,other.indent)

    val content: Option[String] = None
    val innerIndent = 0
}

case object NonToken extends Token{
    val lineNumber = -1
    // Because " " can never be an actual symbol because of
    // the lexer, we use it to denote a token that can't be printed
    val symbol = Some(" ")
    val content = None
    val indent = -1
    val innerIndent = -1
}