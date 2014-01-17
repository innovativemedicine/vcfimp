package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._

import scala.util.parsing.combinator.JavaTokenParsers

import scala.collection.mutable


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
      val genotypeParser = tab ~> genotype(fmts, alleleCount)
      repN(vcfInfo.samples.size, genotypeParser) ^^ { fmts -> _ }
    }
    
  
  /**
   * Given a list of parsers, produce a parser for a list of things, separated
   * by a separator (default ":"). Trailing things may be dropped.
   */
  protected def parseDropTrailing[T](parsers: List[Parser[T]], separator : String = ":") : Parser[List[T]] = parsers match {
    case Nil =>
      // If they give us no parsers, we parse nothing.
      success(Nil)  
    case firstParser :: restParsers =>
      // If they give us a parser and the rest, we definitely parse the first,
      // and then optionally parse the rest.
      Parser[List[T]](in =>
        // Parse the first
        firstParser(in) match {
          // We parsed the first thing successfully.
          case Success(firstResult, rest) => 
            // Try parsing the separator and then the rest.
            (separator ~> parseDropTrailing(restParsers))(rest) match {
              case Success(restResults, after) =>
                // We successfully parsed the rest and got a list for it. Stick
                // our first result on and succeed with that.
                Success(firstResult :: restResults, after)
              case err : NoSuccess =>
                // We only succeeded as far as our first item, but that's OK. Just
                // succeed with that result in a list, and try parsing rest
                // through whatever parser comes next.
                Success(firstResult :: Nil, rest)
          }
          
          // The first thing there did not parse.
          case err : NoSuccess => err
        } 
      )
  }

  protected def valueParserList(parsers: List[Parser[List[VcfValue]]]): Parser[List[List[VcfValue]]] = parseDropTrailing(parsers) 
  
  type FormatKey = (List[Format], Option[Int], Int)
  
  private var cache: mutable.Map[FormatKey, Parser[List[List[VcfValue]]]] =
    new mutable.HashMap[FormatKey, Parser[List[List[VcfValue]]]] with mutable.SynchronizedMap[FormatKey, Parser[List[List[VcfValue]]]]
  
  
  
  def genotype(formats: List[Format], alleleCount: Int): Parser[List[List[VcfValue]]] = {
    
    // What is the minimum length that the list of values can have to include
    // the GT? If "GT" does not exist, this will be 0.
    val minLength = (formats map {_.id.id} indexOf "GT") + 1
    
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
    
    // Make a parser to parse out the number of genotypes
    val gtParser: Option[Int] => Parser[List[List[VcfValue]]] =
      gtCount => cache.getOrElseUpdate((formats, gtCount, alleleCount),
          valueParserList(formats map (getParser(_, gtCount, alleleCount))))
    
    // Make a parser to parse the actual list of VcfValues
    val genotypeParser = Parser(in => gtCountParser(in) match {
      case Success(gtCount, _) => gtParser(gtCount)(in)
      case err: NoSuccess => err
    })
    
    // Return a parser that parses out the value list, but rejects it if it is
    // too short to include the "GT" field.
    Parser(in => genotypeParser(in) match {
      case Success(valueList, rest) => {
        if(valueList.size >= minLength) {
          // We found enough values to have GT
          Success(valueList, rest)
        } else {
          // We dropped GT, which is illegal
          Failure("Illegally dropped \"GT\" field", in)
        }
      }
      // We didn't parse the genotype fields.
      case err : NoSuccess => err
    })
  }
  
  
  lazy val format: Parser[List[Format]] = repsep("[a-zA-Z0-9]+".r, ':') >> { ids =>
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
