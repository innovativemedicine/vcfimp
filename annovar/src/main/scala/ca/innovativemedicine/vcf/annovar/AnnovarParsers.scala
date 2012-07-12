package ca.innovativemedicine.vcf.annovar

import ca.innovativemedicine.vcf._
import ca.innovativemedicine.vcf.parsers._
import ca.innovativemedicine.vcf.format.Patch
import Metadata._


trait AnnovarPatchParsers extends MetadataParsers {
  val newline: Parser[String] = """\r?\n""".r
  
  def patch: Parser[AnnovarPatch.Descriptor] =
    (repsep(metadata, newline) <~ "#TYPE\tID\tCOL" <~ newline) >> {
      case metadata =>
        val infoMap = (metadata collect {
          case info: Info => info.id.id -> info
        }).toMap
        
        (repsep(columnMap, newline) <~ repsep(whitespace, newline)) ^^ { maps =>
          maps.foldLeft((None: Option[Int], Map[Info, Int]())) {
            case ((_, infoCols), ("VCF", _, col)) =>
              (Some(col), infoCols)
            
            case ((vcfCol, infoCols), ("INFO", id, col)) =>
              (vcfCol, infoMap get id map (info => infoCols + (info -> col)) getOrElse infoCols)
              
            case (acc, (tpe, id, col)) =>
              println("Unknown TYPE %s (ID: %s, COL: %d): Ignoring row" format (tpe, id, col))
              acc
          } match {
            case (Some(vcfCol), infoCols) =>
              AnnovarPatch.Descriptor(vcfCol, infoCols)
              
            case (None, _) =>
              throw new IllegalArgumentException("Annovar VCF patches require a row with TYPE `VCF`, but it was not found.")
          }
        }
    }
  
  def columnMap: Parser[(String, String, Int)] = (field <~ tab) ~ (field <~ tab) ~ (wholeNumber ^^ (_.toInt)) ^^ {
    case tpe ~ id ~ col => (tpe, id, col)
  }
}


trait AnnovarTsvParsers extends VariantParsers {
  import AnnovarPatch._
  
  def desc: AnnovarPatch.Descriptor
  
  // Pretty basic VcfInfo, with just the Info needed to create value parsers.
  lazy val vcfInfo = VcfInfo(desc.extraInfoCol.keys.toList, Nil)
  
  // Parses just the info needed to uniquely identify a variant.
  def variantId: Parser[VariantID] =
    (chromosome & position & ids & ref & alternates) ^^ {
      case chrom ~ pos ~ _ ~ ref ~ alts =>
        VariantID(chrom, pos, ref, alts)
    }
  
  /**
   * Returns a parser that can parse a row of an Annovar output file (w/
   * VCF info included).
   */
  def annovarRow: Parser[AnnovarRow] = {
    
    // get a list of all columns
    // Expand list to include ignored columns; Variant will always take up 5
    def parser(cols: List[Option[Info]], col: Int, p: Parser[(Option[VariantID], List[(Info, List[VcfValue])])]): Parser[(Option[VariantID], List[(Info, List[VcfValue])])] = {
      
      // Skip the tab if its the first row.
      val maybeTab: Parser[String] = if (col == 0) success("") else tab
      
      if (col == desc.vcfStartCol) {
        parser(cols drop 5, col + 5, (p <~ maybeTab) >> { case (_, values) =>
          variantId ^^ { Some(_) -> values }
        })
        
      } else (cols match {
        case Some(info) :: cols =>
          parser(cols, col + 1, (p <~ maybeTab) >> { case (v, values) =>
            
            // ANNOVAR files sometimes use ';' to separate multiple values, but
            // an INFO parser won't allow this. The following fix is pretty
            // hacky, so it should probably be rethought.
            
            val infop = repsep(getParser(info, None, 2), ';') ^^ { _.flatMap(identity) }
            infop ^^ { vs => (v, (info, vs) :: values) }
          })
        
        case None :: cols =>
          parser(cols, col + 1, p <~ maybeTab <~ field)
          
        case Nil => p
      })
    }
    
    parser(desc.columnMap, 0, success(None -> Nil)) >> {
      case (Some(v), values) => success(v -> values) <~ rep(tab ~ field)
      case (None, _) => err("No variant info found... This shouldn't happen.")
    }
  }
}