package ca.innovativemedicine.vcf

sealed trait VcfValue

case class VcfInteger(value: Int) extends VcfValue
case class VcfFloat(value: Double) extends VcfValue
case class VcfCharacter(value: Char) extends VcfValue
case class VcfString(value: String) extends VcfValue
case object VcfFlag extends VcfValue