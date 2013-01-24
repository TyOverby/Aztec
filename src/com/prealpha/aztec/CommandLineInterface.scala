package com.prealpha.aztec

import com.prealpha.aztec.beta.generators.{Html, Latex}
import io.Source
import java.io.FileWriter

case class NoSuchGeneratorException(gen: String) extends Exception(gen + " not found")

object CommandLineInterface {
    def getGenerator(input: String):Generator = {
        input.trim.toLowerCase match {
            case "latex" => Latex
            case "html"  => Html
            case _       => throw new NoSuchGeneratorException(input)
        }
    }

    def printHelp(args: Array[String]) {
        println(args.mkString("[",",","]\n\n")+
            """
          | Usage: aztec generator file [ -o outputFile ]
          | Where generator is one of {html, latex}
          | and file is a .aztec file used for input.
          |
          | If the '-o' flag is used, you must provide a file
          | name that the output will be written to.  Otherwise
          | it will be printed to stdout.
        """.stripMargin)
    }

    def main(args: Array[String]) = {
        if (args.length != 2 && args.length != 3){
            printHelp(args)
            sys.exit(1)
        }

        args match {
            case Array(gen, "-")  => println(Transformer.transform(Lexer.lex(Source.stdin),getGenerator(gen)).mkString("\n"))
            case Array(gen, file) => println(Transformer.transform(Lexer.lex(Source.fromFile(file)),getGenerator(gen)).mkString("\n"))
            case Array(gen, "-", "-o", output) =>{
                val out = new FileWriter(output)
                val tt  = Transformer.transform(Lexer.lex(Source.stdin), getGenerator(gen))
                out.write(tt.mkString("\n"))
                out.close()
            }
            case Array(gen, file, "-o", output) => {
                val out = new FileWriter(output)
                val tt  = Transformer.transform(Lexer.lex(Source.fromFile(file)), getGenerator(gen))
                out.write(tt.mkString("\n"))
                out.close()
            }
        }

    }
}
