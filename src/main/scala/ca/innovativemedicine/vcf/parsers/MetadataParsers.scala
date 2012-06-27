package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._
import scala.util.parsing.combinator.JavaTokenParsers


trait MetadataParsers extends JavaTokenParsers {
  import Metadata._
  
  
  // Parsers for arbitrary metadata.
  
  
  def header: Parser[Either[List[Sample], Metadata]] =
    (metadata ^^ (Right(_))) | (samples ^^ (Left(_)))
  
  
  def metadata: Parser[Metadata] = "##" ~> (version | reference | info | filter | format | alt | unhandled)
  
  
  def samples: Parser[List[Sample]] =
    ("#CHROM" ~ "POS" ~ "ID" ~ "REF" ~ "ALT" ~ "QUAL" ~ "FILTER" ~ "INFO" ~ "FORMAT") ~> {
      rep("[^\\t]+".r) ^^ { _ map { id => Sample(VcfId(id)) } }
    }
  
  
  // Parsers for different Metadata types.
  
  
  def version = "fileformat=" ~> ".*".r ^^ { Version(_) }
  
  def reference = "reference=" ~> ".*".r ^^ { Reference(_) }
  
  def unhandled = ".*".r ^^ { Unhandled(_) }
  
  def info = meta("INFO") { _(id, arity, typed, description) } ^^ {
    case id ~ arity ~ tpe ~ desc =>
      Info(id, arity, tpe, Some(desc))
  }
  
  def filter = meta("FILTER") { _(id, description) } ^^ {
    case id ~ desc =>
      Filter(id, Some(desc))
  }
  
  def format = meta("FORMAT") { _(id, arity, shortTyped, description) } ^^ {
    case id ~ arity ~ tpe ~ desc =>
      Format(id, arity, tpe, Some(desc))
  }
  
  def alt = meta("ALT") { _(id, description) } ^^ {
    case id ~ desc =>
      Alt(id, Some(desc))
  }

  
  // Helper/support parsers.
  
  
  def strProp(p: String) = (p + "=") ~> "[^<>,]+".r
  
  def numProp(p: String) = (p + "=") ~> wholeNumber
  
  def quotedProp(p: String) = (p + "=" + "\"") ~> "[^\"]+".r <~ "\""
  
  def types = ("Integer" | "Float" | "Character" | "String") ^^ {
    case "Integer"   => Type.IntegerType
    case "Float"     => Type.FloatType
    case "Character" => Type.CharacterType
    case "String"    => Type.StringType
  }
  
  def typed: Parser[Type] = ("Type=") ~> (types | ("Flag" ^^^ Type.FlagType))
  
  def shortTyped: Parser[RestrictedType] = ("Type=") ~> types
  
  def id = strProp("ID") ^^ { VcfId(_) }
  
  def arity = "Number=" ~> ( ("A" ^^^ Arity.MatchAlleleCount)
		  				   | ("G" ^^^ Arity.MatchGenotypeCount)
		  				   | ("." ^^^ Arity.Variable)
		  				   | (wholeNumber ^^ { n => Arity.Exact(n.toInt) })
		  				   )
  
  def description = quotedProp("Description") | strProp("Description")
  
  protected final class ArgListBuilder {
    def apply[A](a: Parser[A]): Parser[A] = a
    
    def apply[A, B](a: Parser[A], b: Parser[B]): Parser[A ~ B] =
      (a <~ ',') ~ b
    
    def apply[A, B, C](a: Parser[A], b: Parser[B], c: Parser[C]): Parser[A ~ B ~ C] =
      (apply(a, b) <~ ',') ~ c
    
    def apply[A, B, C, D](a: Parser[A], b: Parser[B], c: Parser[C], d: Parser[D]): Parser[A ~ B ~ C ~ D] =
      (apply(a, b, c) <~ ',') ~ d
  }
  
  def meta[A](name: String)(f: ArgListBuilder => Parser[A]): Parser[A] = (name ~> '=' ~> '<') ~> (f(new ArgListBuilder)) <~ '>'
}