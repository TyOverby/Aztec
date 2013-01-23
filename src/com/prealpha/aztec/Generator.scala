package com.prealpha.aztec

import scala.collection.mutable.ArrayStack

trait Generator{
    val startedStack = ArrayStack[Token]()

    def transform(input: Token, previous: Token):String
    val name: String
}
