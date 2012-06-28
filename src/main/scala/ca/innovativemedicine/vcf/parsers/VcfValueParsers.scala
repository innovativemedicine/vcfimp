package ca.innovativemedicine.vcf.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import ca.innovativemedicine.vcf._
import scala.util.matching.Regex


/**
 * Provides a set of parsers for parsing VCF values, such as Integers, Floats,
 * Strings, Characters, and Flags (the last just returns successfully). It also
 * provides a method, `getParser(...)`, that'll return a valid parser (both
 * arity and type) for a given `Metadata with HasArity with HasType`.
 */
trait VcfValueParsers extends JavaTokenParsers {
  import Metadata._
  
  def vcfNonNegativeInteger = "\\d+".r ^^ { n => VcfInteger(n.toInt) }
  def vcfInteger = wholeNumber ^^ { n => VcfInteger(n.toInt) }
  def vcfFloat = floatingPointNumber ^^ { n => VcfFloat(n.toDouble) }
  def vcfCharacter = ".".r ^^ { c => VcfCharacter(c.charAt(0)) }
  def vcfString(pattern: Regex) = pattern ^^ { VcfString(_) }
  def vcfFlag = success(VcfFlag)
  
  
  
  def repNsep[A](n: Int, p: => Parser[A], sep: => Parser[Any]): Parser[List[A]] = if (n == 0) {
    success(Nil)
  } else if (n == 1) {
    p ^^ { List(_) }
  } else {
    (p ~ repN(n - 1, sep ~> p)) ^^ { case x ~ xs => x :: xs }
  }
  
  /**
   * Returns a `Parser` that will parse the `=data[,data]` part of an INFO
   * field. It uses the type and arity information from the `VcfInfo` to ensure
   * correctness and will report an error otherwise.
   * 
   * @param info The metadata INFO field to generate a parser for.
   * @param alleleCount The number of alleles for this variant.
   */
  def getParser(info: Metadata with HasArity with HasType, genotypeCount: Int, alleleCount: Int): Parser[List[VcfValue]] = {
    import Arity._
    
  	val valParser: Parser[VcfValue] = info.typed match {
  	  case Type.IntegerType => vcfInteger
  	  case Type.FloatType => vcfFloat
  	  case Type.StringType => vcfString("[^,;:]*".r)
  	  case Type.CharacterType => vcfCharacter
  	  case Type.FlagType => vcfFlag
  	}
  	  
  	info.arity match {
  	  case MatchAlleleCount => 
  	    repNsep(alleleCount, valParser, ",")
  	
  	  case MatchGenotypeCount =>
  	    repNsep(genotypeCount, valParser, ",")
  
  	  case Variable =>
  	    repsep(valParser, ",")
  
  	  case Exact(n) =>
  	    repNsep(n, valParser, ",")
  	}
  }
}