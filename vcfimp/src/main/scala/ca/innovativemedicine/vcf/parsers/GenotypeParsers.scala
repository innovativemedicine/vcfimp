package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._

import scala.util.parsing.combinator.JavaTokenParsers


/**
 * Provides parsers for genotype data; that is, the FORMAT strings and the
 * sample data (sample data parser generated from the FORMAT string).
 */
trait GenotypeParsers extends TsvParsers with VcfValueParsers {
  import Metadata._
  
  def vcfInfo: VcfInfo
  
  
  /**
   * Parses a FORMAT field, followed by exactly `genotypeCount` genotypes.
   * 
   * The order of the formats and the order of the values in the genotype
   * fields must exactly.
   */
  def genotypes(alleleCount: Int): Parser[(List[Format], List[List[List[VcfValue]]])] =
    format >> { fmts =>
      repN(vcfInfo.samples.size, tab ~> genotype(fmts, alleleCount)) ^^ { fmts -> _ }
    }
    
    
  protected def valueParserList(parsers: List[Parser[List[VcfValue]]]): Parser[List[List[VcfValue]]] = parsers match {
    case Nil =>
      success(List[List[VcfValue]]())
      
    case p :: parsers =>
      parsers.foldLeft(p ^^ { _ :: Nil }) { case (ps, p) =>
        ps >> { vs => (':' ~> p) ^^ { _ :: vs } }
      } ^^ { _.reverse }
  }
  
  
  def genotype(formats: List[Format], alleleCount: Int): Parser[List[List[VcfValue]]] = {
    
    // If any of the formats require the genotype count (Number=G), then we'll
    // first parse the data while ignoring arity to see if we can determine the
    // genotype count from the GT field. Once we get this, we then re-parse the
    // data, using the arity information. If the GT field isn't present, but a
    // field requiring the genotype count is, then the parse will fail.
    
    val gtCountParser: Parser[Option[Int]] = if (formats exists (_.arity == Arity.MatchGenotypeCount)) {
      formats find (_.id.id == "GT") map { _ =>
        val parsers = valueParserList(for {
          fmt <- formats
        } yield getParser(fmt.copy(arity = Arity.Variable), None, alleleCount))
        
        parsers ^^ { values =>
          (formats zip values) collectFirst { case (Format(VcfId("GT"), _, _, _), VcfString(gt) :: Nil) =>
            (gt split "[/|]").size
          }
        }
      } getOrElse (success(None))
    } else success(None)
    
    val gtParser: Option[Int] => Parser[List[List[VcfValue]]] =
      gtCount => formats map (getParser(_, gtCount, alleleCount)) match {
        case Nil =>
          success(List[List[VcfValue]]())
          
        case p :: parsers =>
          parsers.foldLeft(p ^^ { _ :: Nil }) { case (ps, p) =>
            ps >> { vs => (':' ~> p) ^^ { _ :: vs } }
          } ^^ { _.reverse }
      }
    
    Parser(in => {
      gtCountParser(in) match {
        case Success(gtCount, _) =>
          gtParser(gtCount)(in)
        case err: NoSuccess => err
      }
    })
  }
  
  
  def format: Parser[List[Format]] = repsep("[a-zA-Z0-9]+".r, ':') >> { ids =>
    val res = ids.foldLeft(Right(Nil): Either[List[String], List[Format]]) {
      case (Right(fmts), id) =>
        vcfInfo.getTypedMetadata[Format](VcfId(id)) map (f => Right(f :: fmts)) getOrElse (Left(id :: Nil))
        
      case (Left(ids), id) =>
        vcfInfo.getTypedMetadata[Format](VcfId(id)) map (_ => Left(ids)) getOrElse (Left(id :: ids))
    }
    
    res match {
      case Left(ids) => err("Could not find FORMAT descriptions for: %s" format (ids.reverse mkString ", "))
      case Right(fmts) => success(fmts.reverse)
    }
  }
}