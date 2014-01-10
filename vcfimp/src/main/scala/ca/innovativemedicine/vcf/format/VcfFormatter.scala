package ca.innovativemedicine.vcf.format

import ca.innovativemedicine.vcf._

/**
 * Helper methods for writing out VCF-related values in a VCF compatible format.
 */
object VcfFormatter {
  import Metadata._
  
  val formatHeader = List("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT") mkString "\t"
  
  def formatMetadata(m: Metadata) = m match {
    case Unhandled(x) => x
    case Version(ver) => "##fileformat=" + ver
    case Reference(ref) => "##reference=" + ref
    
    case Filter(id, desc) =>
      "##FILTER=<ID=%s,Description=\"%s\">" format (id.id, desc getOrElse "")
      
    case Format(id, a, tpe, desc) =>
      "##FORMAT=<ID=%s,Number=%s,Type=%s,Description=\"%s\">" format (id.id, formatArity(a), formatType(tpe), desc getOrElse "")
      
    case Info(id, a, tpe, desc) =>
      "##INFO=<ID=%s,Number=%s,Type=%s,Description=\"%s\">" format (id.id, formatArity(a), formatType(tpe), desc getOrElse "")
      
    case Alt(id, desc) =>
      "##ALT=<ID=%s,Description=\"%s\">" format (id.id, desc getOrElse "")
  }
  
  def formatVcfValue(value: VcfValue) = value match {
    case VcfInteger(value: Int) => value.toString
    case VcfFloat(value: Double) => value.toString
    case VcfCharacter(value: Char) => value.toString
    case VcfString(value: String) => value
    case VcfFlag => ""
    case VcfMissing => "."
  }
  
  def formatBreakend(breakend: Breakend) = breakend.toBreakendString
  
  def formatChromosome(chr: Variant.Chromosome) = chr.fold(_.toString, identity)
  
  def formatAlternate(alt: Variant.Alternate) = alt.fold(_.fold(_.toBreakendString, _.toString), identity)
  
  def formatFilterResult(fr: FilterResult) = fr match {
    case FilterResult.Pass => "PASS"
    case FilterResult.Fail(reasons) => reasons map (_.id.id) mkString ";"
  }
  
  def formatArity(a: Arity): String = a match {
    case Arity.Exact(n) => n.toString
    case Arity.Variable => "."
    case Arity.MatchAlleleCount => "A"
    case Arity.MatchGenotypeCount => "G"
  }
  
  def formatType(t: Type): String = t match {
    case Type.CharacterType => "Character"
    case Type.StringType => "String"
    case Type.IntegerType => "Integer"
    case Type.FloatType => "Float"
    case Type.FlagType => "Flag"
  }
}
