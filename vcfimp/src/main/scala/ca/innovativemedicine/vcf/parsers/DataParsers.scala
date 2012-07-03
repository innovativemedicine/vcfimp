package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._


trait DataParsers extends VariantParsers with GenotypeParsers {
  import Metadata._
  
  lazy val genotypeCount = vcfInfo.samples.size
  
  /**
   * Parses an entire row of a VCF file and obtains the variant information,
   * the list of genotype formats, and the actual list of genotype information
   * for each sample.
   */
  def row: Parser[(Variant, List[Format], List[List[List[VcfValue]]])] =
    variant >> { variant =>
      val alleleCount = variant.alternates.size
      
      tab ~> genotypes(genotypeCount, alleleCount) ^^ {
        case (fmts, gts) =>
          (variant, fmts, gts)
      }
    }
}