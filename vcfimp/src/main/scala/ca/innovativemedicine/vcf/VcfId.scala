package ca.innovativemedicine.vcf

/**
 * VCF IDs are used in VCF files to reference values define in the meta
 * information of the file. This can only be useful within the context of a VCF
 * file.
 */
case class VcfId(id: String) {
  override def toString: String = "<" + id + ">"
}

object VcfId {
  
  /** Pattern matcher for VCF IDs. Will return ID without angle brackets. */
  val Validated = """<([^<>]*)>""" r
}
