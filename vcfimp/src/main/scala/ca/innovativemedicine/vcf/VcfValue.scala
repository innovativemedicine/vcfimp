package ca.innovativemedicine.vcf

sealed trait VcfValue

case class VcfInteger(value: Int) extends VcfValue
case class VcfFloat(value: Double) extends VcfValue
case class VcfCharacter(value: Char) extends VcfValue
case class VcfString(value: String) extends VcfValue
case object VcfFlag extends VcfValue
// VCF needs to support "missing data" as a special value (sometimes an
// "integer" field can have "." in it.)
case object VcfMissing extends VcfValue
