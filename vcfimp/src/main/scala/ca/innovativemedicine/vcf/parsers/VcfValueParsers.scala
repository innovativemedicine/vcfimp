package ca.innovativemedicine.vcf.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import ca.innovativemedicine.vcf._
import scala.util.matching.Regex

import scala.collection.mutable


/**
 * Provides a set of parsers for parsing VCF values, such as Integers, Floats,
 * Strings, Characters, and Flags (the last just returns successfully). It also
 * provides a method, `getParser(...)`, that'll return a valid parser (both
 * arity and type) for a given `Metadata with HasArity with HasType`.
 */
trait VcfValueParsers extends JavaTokenParsers {
  import Metadata._
  
  // Parsers for basic VCF value types.
  // Missing data
  val vcfMissing = "\\.".r ^^ { _ => VcfMissing }
  // A non-missing non-negative int
  val vcfNonNegativeInteger = "\\d+".r ^^ { n => VcfInteger(n.toInt) }
  // A non-missing int
  val vcfInteger = wholeNumber ^^ { n => VcfInteger(n.toInt) }
  // A non-missing float
  val vcfFloat = floatingPointNumber ^^ { n => VcfFloat(n.toDouble) }
  // A character (which is not ".", because "." means "missing")
  val vcfCharacter = "[^.]".r ^^ { c => VcfCharacter(c.charAt(0)) }
  // A string (which is not ".", because "." would mean "missing")
  def vcfString(pattern: Regex) = {
    // We need to parse the string with pattern (a parser), then check if it's
    // "." and fail if so. We need to do all that inside a Parser.
    Parser[VcfValue](in => pattern(in) match {
      // We succesfully matched just a .
      case Success(".", rest) => Failure("Non-missing string may not be just \".\"", in)
      // We successfully parsed some other string. Wrap it in a VcfString.
      case Success(string, rest) => Success(VcfString(string), rest)
      case err: NoSuccess => err
    })
  }
  
  // A flag, which has no actual value
  val vcfFlag = success(VcfFlag)
  
  
  /**
   * Like repsep, but specifies the exact number expected. I am not sure why
   * this was omitted from `scala.util.parsing.combinator.Parsers`.
   */
  def repNsep[A](n: Int, p: => Parser[A], sep: => Parser[Any]): Parser[List[A]] = if (n == 0) {
    success(Nil)
  } else if (n == 1) {
    p ^^ { List(_) }
  } else {
    (p ~ repN(n - 1, sep ~> p)) ^^ { case x ~ xs => x :: xs }
  }
  
  
  type MetadataKey = (Metadata with HasArity with HasType, Option[Int], Int)
  
  private var cache: mutable.Map[MetadataKey, Parser[List[VcfValue]]] =
    new mutable.HashMap[MetadataKey, Parser[List[VcfValue]]] with mutable.SynchronizedMap[MetadataKey, Parser[List[VcfValue]]]
  
  
  /**
   * Returns a `Parser` that will parse the `=data[,data]` part of an INFO
   * field. It uses the type and arity information from the `VcfInfo` to ensure
   * correctness and will report an error otherwise.
   * 
   * This can optionally take the ploidy (genotypeCount) and the number of
   * alternate alleles. The ploidy would only be available for a sample AND if
   * the GT field is present. If the `Arity` of `info` is `MatchGenotypeCount`
   * and `genotypeCount` is `None`, then this will result in a `Parser` that
   * always fails (and helpfully indicates that not genotype info was present).
   * 
   * This function also produces parsers for FORMAT (genotype) field values.
   *
   * Anything that isn't a flag may parse as a `VcfMissing`; for example, a
   * character field with the value "." will parse as `VcfMissing` rather than
   * `VcfCharacter(".")`.
   *
   * @param info The metadata INFO or FORMAT field to generate a parser for.
   * @param genotypeCount An optional "genotype" count (the ploidy).
   * @param alleleCount The number of alleles for this variant.
   */
  def getParser(info: Metadata with HasArity with HasType, genotypeCount: Option[Int], alleleCount: Int): Parser[List[VcfValue]] = {
    import Arity._
    
    println("Getting parser for %s".format(info))
    
    cache.getOrElseUpdate((info, genotypeCount, alleleCount), {
    	val valParser: Parser[VcfValue] = info.typed match {
    	  case Type.IntegerType => vcfInteger | vcfMissing
    	  case Type.FloatType => vcfFloat | vcfMissing
    	  case Type.StringType => vcfString(info match {
    	      case _: Info => "[^,;\\t]*".r
    	      case _: Format => "[^,:\\t]*".r
    	    }) | vcfMissing
    	  case Type.CharacterType => vcfCharacter | vcfMissing
    	  case Type.FlagType => vcfFlag
    	}
    	  
    	info.arity match {
    	  case MatchAlleleCount =>
    	    repNsep(alleleCount, valParser, ",")
    	
    	  case MatchGenotypeCount =>
    	    genotypeCount map (repNsep(_, valParser, ",")) getOrElse err("Cannot get parser with arity G: No genotype information available here.")
    
    	  case Variable =>
    	    repsep(valParser, ",")
    
    	  case Exact(n) =>
    	    repNsep(n, valParser, ",")
    	}
    })
  }
}
