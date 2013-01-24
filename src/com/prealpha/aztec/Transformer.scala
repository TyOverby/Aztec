package com.prealpha.aztec

import beta.generators.Latex
import com.prealpha.aztec.Lexer.lex
import io.Source
import scala.collection.immutable.Stack

object Transformer{

    def transform(tokens: List[Token], gen: Generator):List[String] = {
        def innert(ts: List[Token], stack: Stack[Token], acc: List[String]): List[String] = {
            if (ts.isEmpty || stack.isEmpty){
                return acc
            }
            val top = stack.top
            val head = ts.head

            (top.indent, head.indent) match {
                // The indent at the head is the same
                // as the indent on our stack.
                case (t, h) if t == h =>
                    // If they are the same symbol, add
                    // the generated line and continue
                    // without modifying the stack.
                    if (top.symbol == head.symbol){
                        val added = gen.gen(head)
                        innert(ts.tail, stack, acc :+ added)
                     } else {
                        // Otherwise, End the current top and
                        // start another one for the head.
                        // Add the current generated and continue
                        // without the previous one on the stack,
                        // but with our current head on it.

                        val added = List(gen.genEnd(top.symbol),
                                         gen.genStart(head.symbol),
                                         gen.gen(head))
                        innert(ts.tail, stack.pop.push(head), added ++ acc)
                     }
                // If the head is greater than the top, we need
                // to start a new starting point and add our
                // generated head.  Add the new head to the stack
                case (t, h) if t < h => {
                    val added = List(gen.genStart(head.symbol),
                                    gen.gen(head))
                    innert(ts.tail, stack.push(head),acc ++ added)
                }
                // If the head is less than the top, then we need to close top
                // and try again at one level below
                case (t, h) if t > h => {
                    val added = gen.genEnd(top.symbol)
                    innert(ts, stack.pop, acc :+ added)
                }
            }
        }

        val firstPass = innert(tokens ++ List(NonToken), Stack(NonToken), Nil)

        gen.postProcess((gen.documentStart(firstPass)++firstPass++gen.documentEnd(firstPass)).toList)
    }


    def main(args: Array[String]) {
        println (transform(lex(Source.fromFile("testing.aztec")), Latex).mkString("\n"))
    }
}
