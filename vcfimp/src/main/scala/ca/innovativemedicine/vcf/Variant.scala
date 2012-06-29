package ca.innovativemedicine.vcf


object Variant {
  
  /** A Chromosome can either be a VCF ID (eg. `<ABC>`) or a simple string. */
  type Chromosome = Either[VcfId, String]
  
  /**
   * An alternate can be a sequence, a VCF ID (eg. `<ABC>`), or a breakend
   * string (eg. `G]17:198982]`).
   */
  type Alternate = Either[Either[Breakend, VcfId], String]
}

case class Variant(
	  chromosome: Variant.Chromosome,
	  position: Int,
	  ids: List[String],
	  reference: String,
	  alternates: List[Variant.Alternate],
	  quality: Option[Double],
	  filter: Option[FilterResult],
	  info: Map[Metadata.Info, List[VcfValue]]
    ) {
  require(position > 0)
}


/*
sealed trait Nucleotide
object Nucleotide {
  val N: Nucleotide = new Nucleotide {}
  case object A extends Nucleotide
  case object T extends Nucleotide
  case object C extends Nucleotide
  case object G extends Nucleotide
  
  type Sequence = List[Nucleotide]
}
*/