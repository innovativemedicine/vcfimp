package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._

import scala.util.parsing.combinator.JavaTokenParsers


/**
 * Provides a suite of parsers that are able to parse parts of the variants
 * from a VCF file.
 */
trait VariantParsers extends TsvParsers with InfoParsers {
  import Variant._
  import FilterResult._
  import JoinType._
  
  
  // Parsers for parsing a whole variant from a row of a VCF file.
  
  
  def variant: Parser[Variant] =
    (chromosome & position & ids & ref & alternates & optional(quality) & optional(filters)) >> {
      
  	  case chr ~ pos ~ ids ~ ref ~ alt ~ qual ~ ftr =>
  	    val alleleCount = 1 + alt.size
  	    (tab ~> info(alleleCount)) ^^ { infoMap =>
  	      Variant(chr, pos, ids, ref, alt, qual, ftr, infoMap)
  	    }
  	    
  	}
    
  
  // Parsers for parsing individual fields from the variant-portion of a VCF file row.
  
  
  def chromosome: Parser[Chromosome] = (vcfId ^^ { Left(_) }) | (name ^^ { Right(_) })
  
  def position: Parser[Int] = "\\d+".r ^^ { _.toInt }
  
  def ids: Parser[List[String]] = repsep("[^\\s;]+".r, ";")
  
  def ref: Parser[String] = sequence
  
  def alternates: Parser[List[Alternate]] = rep1sep(
      (sequence ^^ { Right(_) })
    | (vcfId ^^ { id => Left(Right(id)) })
    | (breakend ^^ { bnd => Left(Left(bnd)) })
    , ',') 
  
  def quality: Parser[Double] = floatingPointNumber ^^ { _.toDouble }
  
  def filters: Parser[FilterResult] = "PASS" ^^^ Pass | repsep("[^\\s;]+", ";") ^^ { Fail(_) }
  
  
  // Support/helper parsers.
  
  
  /** Indicates a value can be replaced with the "missing value" (ie. '.'). */
  def optional[A](p: => Parser[A]): Parser[Option[A]] = "." ^^^ None | p ^^ { Some(_) }
  
  def vcfId = "<" ~> ("[^<>]+".r) <~ ">" ^^ { VcfId(_) }
  
  def name: Parser[String] = "\\S+".r
  
  def sequence: Parser[String] = "[atcgnATCGN]+".r ^^ { _.toUpperCase() }
  
  def chromosomePosition: Parser[Chromosome ~ Int] = (chromosome <~ ':') ~ position
  
  def breakend: Parser[Breakend] = (sequence ~ ("[" ~> chromosomePosition <~ "[") ^^
								     { case s ~ (c ~ p) => Breakend(s, c, p, JoinAfter) }
  
								   | ("]" ~> chromosomePosition <~ "]") ~ sequence ^^
								  	 { case c ~ p ~ s => Breakend(s, c, p, JoinBefore) }
								   
								   | sequence ~ ("]" ~> chromosomePosition <~ "]") ^^
								  	 { case s ~ (c ~ p) => Breakend(s, c, p, JoinReverseAfter) }
								   
								   | ("[" ~> chromosomePosition <~ "[") ~ sequence ^^
								     { case c ~ p ~ s => Breakend(s, c, p, JoinReverseBefore) }
								   )

}


object VariantParsers {
  def apply(vcf: VcfInfo): VariantParsers = new VariantParsers { val vcfInfo = vcf }
}
