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
  
  val vcfNonNegativeInteger = "\\d+".r ^^ { n => VcfInteger(n.toInt) }
  val vcfInteger = wholeNumber ^^ { n => VcfInteger(n.toInt) }
  val vcfFloat = floatingPointNumber ^^ { n => VcfFloat(n.toDouble) }
  val vcfCharacter = ".".r ^^ { c => VcfCharacter(c.charAt(0)) }
  def vcfString(pattern: Regex) = pattern ^^ { VcfString(_) }
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
   * @param info The metadata INFO field to generate a parser for.
   * @param genotypeCount An optional "genotype" count (the ploidy).
   * @param alleleCount The number of alleles for this variant.
   */
  def getParser(info: Metadata with HasArity with HasType, genotypeCount: Option[Int], alleleCount: Int): Parser[List[VcfValue]] = {
    import Arity._
    
    cache.getOrElseUpdate((info, genotypeCount, alleleCount), {
    	val valParser: Parser[VcfValue] = info.typed match {
    	  case Type.IntegerType => vcfInteger
    	  case Type.FloatType => vcfFloat
    	  case Type.StringType => vcfString(info match {
    	      case _: Info => "[^,;\\t]*".r
    	      case _: Format => "[^,:\\t]*".r
    	    })
    	  case Type.CharacterType => vcfCharacter
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