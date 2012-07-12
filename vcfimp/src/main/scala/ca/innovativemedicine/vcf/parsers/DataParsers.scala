package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._


trait DataParsers extends VariantParsers with GenotypeParsers {
  import Metadata._
  
  /**
   * Parses an entire row of a VCF file and obtains the variant information,
   * the list of genotype formats, and the actual list of genotype information
   * for each sample.
   */
  lazy val row: Parser[(Variant, List[Format], List[List[List[VcfValue]]])] =
    variant >> { variant =>
      tab ~> genotypes(variant.alleleCount) ^^ {
        case (fmts, gts) =>
          (variant, fmts, gts)
      }
    }
}