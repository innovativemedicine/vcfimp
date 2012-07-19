package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._


trait DataParsers extends VariantParsers with GenotypeParsers { parsers =>
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
    
    
  // The stuff below is not the same as ca.innovativemedicine.vcf.VcfRow, which
  // is just a type alias for `(Variant, List[Format], List[List[List[VcfValue]]])`.
  // Instead, below would be a replacement for it, should you choose to use it. The
  // main gain is that we add a new PartialVcfRow which represents a VcfRow where
  // only the variant and formats have been parsed and NOT the sample data. This
  // would save some time when parsing the 1000G data, as there are so many samples
  // per VCF. However, it won't do much for files with a handful of samples. Since
  // I'm guessing these would actually be more common once the pipelien is up and
  // running I just left this here, unused. However, it could be useful in a more
  // general sense, if other people start using this.
    
    
  /**
   * This will parse the variant and format information, but defers the parse
   * of the sample data. This means errors in the sample data won't be caught
   * at parse time, but only when `values` is accessed in the returned
   * `VcfRow`, at which point it just throws an `Exception`.
   * 
   * Note though, that this parse will pretend it parsed the full line. So,
   * combining this with other parses will work just fine.
   */
  lazy val partialRow: Parser[VcfRow] = Parser(in => {
    val fixed = (variant & format) <~ tab <~ ".*".r ^^ { case variant ~ fmts =>
      (variant, fmts)
    }
    
    fixed(in) match {
      case Success((variant, fmts), rest) =>
        val genotypeParser = tab ~> genotype(fmts, variant.alleleCount)
        val variable = repNsep(9, field, tab) ~> repN(vcfInfo.samples.size, genotypeParser)
        Success(VcfRow.partial(variant, fmts, variable(in) match {
          case Success(values, _) => Right(values)
          case error: NoSuccess => Left(error.toString)
        }), rest)
        
      case error: NoSuccess => error
    }
  })
    
  // Fast row parsers:
  // Parse all fields as strings in the TSV. Only parse on demand.
  // Need to be able to patch it without evaluation.
    
  sealed trait VcfRow {
    def variant: Variant
    def formats: List[Format]
    def values: List[List[List[VcfValue]]]
    
    def canEqual(that: Any) = that match {
      case row: VcfRow => true
      case _ => false
    }
    
    def equals(that: VcfRow): Boolean = (that canEqual this) && (that match {
      case that: VcfRow =>
        that.variant == this.variant && that.formats == this.formats && that.values == this.values
      case _ =>
        false
    })
    
    def copy(variant: Variant = variant, formats: List[Format] = formats, values: List[List[List[VcfValue]]] = values): VcfRow
    
    def tupled = (variant, formats, values)
    
    def strict: Either[String, StrictVcfRow]
  }
  
  object VcfRow {
    def unapply(row: VcfRow): Some[(Variant, List[Format], List[List[List[VcfValue]]])] = Some(row.tupled)
    
    def apply(variant: Variant, formats: List[Format], values: List[List[List[VcfValue]]]) =
      new StrictVcfRow(variant, formats, values)
    
    def partial(variant: Variant, formats: List[Format], values: => Either[String, List[List[List[VcfValue]]]]) =
      new PartialVcfRow(variant, formats, () => values)
  }
  
  
  final class StrictVcfRow(val variant: Variant, val formats: List[Format], val values: List[List[List[VcfValue]]]) extends VcfRow {
    def copy(variant: Variant = variant, formats: List[Format] = formats, values: List[List[List[VcfValue]]] = values): VcfRow =
      new StrictVcfRow(variant, formats, values)
    
    def strict = Right(this)
  }
  
  final class PartialVcfRow(val variant: Variant, val formats: List[Format], parseValues: () => Either[String, List[List[List[VcfValue]]]]) extends VcfRow {
    @volatile private var parsed = false
    private lazy val _values = {
      val result = parseValues()
      parsed = true
      result
    }
    
    def values = _values match {
      case Left(error) => throw new IllegalStateException(error)
      case Right(values) => values
    }
    
    def copy(variant: Variant = variant, formats: List[Format] = formats, values: List[List[List[VcfValue]]] = null) =
      if (values == null && !parsed) {
        new PartialVcfRow(variant, formats, parseValues)
      } else {
        new StrictVcfRow(variant, formats, values)
      }
    
    def strict = _values.right map (new StrictVcfRow(variant, formats, _))
  }
}