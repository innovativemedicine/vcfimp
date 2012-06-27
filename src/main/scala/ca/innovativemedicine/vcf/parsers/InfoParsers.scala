package ca.innovativemedicine.vcf.parsers

import scala.util.parsing.combinator.JavaTokenParsers
import ca.innovativemedicine.vcf._


/**
 * This is a parser that can be used to parse the INFO field from a VCF file.
 * It will use the metadata from the file (in a `VcfInfo`) to ensure the exact
 * arity and types match.
 */
trait InfoParsers extends JavaTokenParsers with VcfValueParsers {
  import Metadata._
  
  def vcfInfo: VcfInfo
  
  // TODO: Handle flags, which may not have the '='.

  private lazy val genotypeCount = vcfInfo.samples.size
  
  def infoField(alleleCount: Int) = "[^=,;]+".r >> { id =>
    vcfInfo.getTypedMetadata[Info](VcfId(id)) match {
      case Some(info) =>
        val p = getParser(info, genotypeCount, alleleCount)
        
        (info.arity match {
          case Arity.Exact(n) if n == 0 =>
            success(Nil)
          
          case Arity.Variable =>
            ('=' ~> p) | success(Nil)
            
          case _ =>
            '=' ~> p

        }) ^^ { info -> _ }
    
      case None =>
        err("Undefined INFO field: <%s>" format id)
    }
  }
  
  def info(alleleCount: Int): Parser[Map[Info, List[VcfValue]]] = repsep(infoField(alleleCount), ';')  ^^ { _.toMap }
}