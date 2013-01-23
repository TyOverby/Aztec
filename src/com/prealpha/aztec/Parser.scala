package com.prealpha.aztec


object Parser {
    def parse(tokenList: List[Token]): Tree = {
        def transformBlanks(list:List[Token]): List[Token] = list match{
            case Nil => Nil
            case EmptyLine(lineNum) :: xs => EmptyWithIndent(lineNum, 0) :: transformBlanks(xs)
            case x :: EmptyLine(lineNum) :: xs => x :: transformBlanks(EmptyWithIndent(lineNum, x.indent) :: xs)
            case x :: xs => x :: transformBlanks(xs)
        }

        Root(parse(NonToken, transformBlanks(tokenList)))
    }

    private[this]
    def parse(parent: Token, tokenList: List[Token]): List[Tree] = {
        def topLevel(tokens: List[Token]): List[Token] = tokens match {
            case Nil => Nil
            case x :: xs => x :: xs.filter(_.indent == x.indent)
        }

        val inside = tokenList.takeWhile(_.indent > parent.indent)
        val tops = topLevel(inside)

        tops.map(x=> Node(x, parse(x, inside.drop(inside.indexOf(x)+1))))
    }

    private[this]
    def contiguous(xs:List[Tree], parent: Tree): Tree = {
        def pullGroup(ys: List[Tree]) = {
            val grouped = ys match{
                case Nil => Nil
                case y :: ys => y :: ys.takeWhile(_.token.symbol == y.token.symbol)
            }
            val rest = ys.drop(grouped.length)
            (grouped, rest)
        }
        def formGroups(xxs: List[Tree]): List[List[Tree]] = {
            pullGroup(xxs) match {
                case (Nil, Nil) => Nil
                case (group, Nil) => group :: Nil
                case (group, rest) => group :: formGroups(rest)
                case (Nil, rest) => sys.error("")
            }
        }
        def wrapToken(t: Token) = ParentToken(t.symbol, t.lineNumber, t.indent)

        val grouped = formGroups(xs)
        val changed = grouped.map(x => Node(wrapToken(x.head.token), x.map(y => contiguous(y.children,y))) )
        Node(parent.token, changed)
    }

    def insertParents(p: Tree) = {
        contiguous(p.children,p)
    }
}
