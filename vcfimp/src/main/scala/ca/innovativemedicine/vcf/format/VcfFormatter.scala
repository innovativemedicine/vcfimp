package ca.innovativemedicine.vcf.format

import ca.innovativemedicine.vcf._

/**
 * Helper methods for writing out VCF-related values in a VCF compatible format.
 */
object VcfFormatter {
  def formatVcfValue(value: VcfValue) = value match {
    case VcfInteger(value: Int) => value.toString
    case VcfFloat(value: Double) => value.toString
    case VcfCharacter(value: Char) => value.toString
    case VcfString(value: String) => value
    case VcfFlag => ""
  }
  
  def formatBreakend(breakend: Breakend) = breakend.toBreakendString
  
  def formatChromosome(chr: Variant.Chromosome) = chr.fold(_.toString, identity)
  
  def formatAlternate(alt: Variant.Alternate) = alt.fold(_.fold(_.toBreakendString, _.toString), identity)
  
  def formatFilterResult(fr: FilterResult) = fr match {
    case FilterResult.Pass => "PASS"
    case FilterResult.Fail(reasons) => reasons mkString ";"
  }
}