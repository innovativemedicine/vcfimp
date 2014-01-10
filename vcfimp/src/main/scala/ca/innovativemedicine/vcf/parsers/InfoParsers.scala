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
  
  // TODO: Handle flags, which may not have the '='... though if they have
  // Number=0, which Flags should, then they will be handled correctly.
  
  def infoField(alleleCount: Int) = "[^=,;\\t]+".r >> { id =>
    vcfInfo.getTypedMetadata[Info](VcfId(id)) match {
      
      case Some(info) if info.typed == Type.FlagType =>
        success(info -> (VcfFlag :: Nil))
        
      case Some(info) =>
        // FIXME: Currently it is an error to use an INFO field with Number=G,
        // as I don't have access to this information (hence `None` is passed
        // to `getParser`). However, the spec isn't clear, and perhaps we are
        // suppose to assume 2. In lieu of more info, errors are appropriate.
        
        val p = getParser(info, None, alleleCount)
        
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
  
  def info(alleleCount: Int): Parser[Map[Info, List[VcfValue]]] =
    repsep(infoField(alleleCount), ';')  ^^ { _.toMap }
}
