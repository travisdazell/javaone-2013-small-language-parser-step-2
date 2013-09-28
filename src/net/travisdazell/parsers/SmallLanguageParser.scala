package net.travisdazell.parsers

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import net.travisdazell.parsers.model._

class SmallLanguageParser extends StandardTokenParsers {
  var currentScope = new Scope("global", null)

  lexical.reserved += ("var", "println", "loop", "times", "endloop", "if", "then", "else", "endif")
  lexical.delimiters += ("*", "/", "%", "+", "-", "(", ")", "=", "<", ">", "==", "!=", "<=", ">=")

  def program: Parser[List[Statement]] = rep(statement) ^^ {
    s =>
      {
        if (!currentScope.name.equalsIgnoreCase("global")) {
          currentScope = currentScope.parentScope
        }

        s
      }
  }

  def statement: Parser[Statement] = variableAssignment | outStatement |
    loopStatement | ifStatement ^^ { case a => a }

  def variableAssignment: Parser[VariableDefinition] = "var" ~> ident ~ "=" ~ expr ^^ {
    case a ~ "=" ~ b => {
      val x = new VariableDefinition(a, b)
      currentScope.variables(a) = b
      x
    }
  }

  def outStatement: Parser[PrintStatement] = "println" ~> expr ^^ {
    case a => new PrintStatement(a)
  }

  def loopStatement: Parser[LoopStatement] = ("loop" ~> iterations <~ "times") ~
    program <~ "endloop" ^^ {
      case i ~ s => {
        new LoopStatement(i, s)
      }
    }

  def ifStatement: Parser[IfStatement] = conditional ~ program ~
    opt("else" ~> program) <~ "endif" ^^ {
      case a ~ b ~ c => {
        c match {
          case None => new IfStatement(a, b, List())
          case _ => new IfStatement(a, b, c.get)
        }
      }
    }

  def conditional: Parser[Boolean] = "if" ~ "(" ~> condition <~ ")" ~ "then"

  def condition: Parser[Boolean] = expr ~
    ("<" | ">" | "==" | "!=" | "<=" | ">=") ~ expr ^^ {
      case a ~ b ~ c => {
        currentScope = new Scope("", currentScope)

        b match {
          case "<" => (a < c)
          case ">" => (a > c)
          case "==" => (a == c)
          case "!=" => (a != c)
          case "<=" => (a <= c)
          case ">=" => (a >= c)
        }
      }
    }

  def iterations: Parser[Int] = numericLit ^^ {
    n =>
      {
        currentScope = new Scope("", currentScope)

        n.toInt
      }
  }

  def expr: Parser[Int] = term ~ rep(("+" | "-") ~ term ^^ {
    case "+" ~ t => t
    case "-" ~ t => -t
  }) ^^ { case t ~ r => t + r.sum }

  def term: Parser[Int] = multiplydividemodulo | factor ^^ { _.toInt }

  def multiplydividemodulo: Parser[Int] = factor ~
    rep(("*" | "/" | "%") ~ factor) ^^ {
      case a ~ List() => a
      case a ~ b => {
        var result = a

        for (f <- b) {
          result =
            f._1 match {
              case "*" => result * f._2
              case "/" => result / f._2
              case "%" => result % f._2
            }
        }

        result
      }
    }

  def factor: Parser[Int] = numericLit ^^ { _.toInt } |
    "(" ~> expr <~ ")" ^^ { e => e } |
    ident ^^ {
      case i => {
        var s: Scope = currentScope

        while ((!s.name.equals("global")) && !s.variables.contains(i)) {
          s = s.parentScope
        }

        if (s.variables.contains(i)) {
          s.variables(i)
        } else {
          sys.error("Undefined variable " + i + " at position [" +
            lastNoSuccess.next.pos.column + "] on line: " +
            lastNoSuccess.next.pos.line)
        }
      }
    }

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = {
    val input = new lexical.Scanner(in)

    val result: ParseResult[T] =
      try {
        phrase(p)(input)
      } catch {
        case e: RuntimeException => this.Error(e.getMessage, input)
      }

    result
  }
}