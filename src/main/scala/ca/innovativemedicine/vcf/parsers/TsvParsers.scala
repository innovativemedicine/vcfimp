package ca.innovativemedicine.vcf.parsers

import scala.util.parsing.combinator.JavaTokenParsers


/**
 * A mix-in for parsers dealing with TSV-like files. This is basically a
 * `JavaTokenParsers`, but with tabs removed from allowed whitespace, and a
 * few convenience methods. This has the parser `tab` which just parses a tab.
 * It also adds 2 identical methods to parsers; `t` and `&`, which can be used
 * to indicate to 2 parsers are separated by a tab. For example, `a & b & c`.
 */
trait TsvParsers extends JavaTokenParsers {
  
  // Omit tabs from normal whitespace skips.
  
  override protected val whiteSpace = """[ \n\x0B\f\r]""".r
  
  protected val tab: Parser[String] = "\t"
  
  
  final class FieldOps[A](a: Parser[A]) {
    def t[B](b: Parser[B]) = (a <~ tab) ~ b
    def &[B](b: Parser[B]) = (a <~ tab) ~ b
  }
  
  implicit def FieldOps[A](a: Parser[A]) = new FieldOps(a)
}