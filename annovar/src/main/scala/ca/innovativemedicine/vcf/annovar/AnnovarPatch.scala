package ca.innovativemedicine.vcf.annovar

import ca.innovativemedicine.vcf._
import ca.innovativemedicine.vcf.parsers._
import ca.innovativemedicine.vcf.format.Patch
import java.io.{ File, FileReader, InputStream, InputStreamReader, BufferedReader }
import Metadata._



object AnnovarPatch {
  case class Descriptor(vcfStartCol: Int, extraInfoCol: Map[Info, Int]) {
    
    lazy val columnMap: List[Option[Info]] = {
      val (len, cols) = (extraInfoCol.toList sortBy (_._2)).foldLeft(0 -> List[Option[Info]]()) {
        case ((len, cols), (info, col)) =>
          (col + 1, Some(info) :: (List.fill(col - len)(None) ++ cols))
      }
      
      cols.reverse ++ List.fill(math.max(vcfStartCol + 5 - len, 0))(None)  
    }
    
  }
  
  
  type AnnovarRow = (VariantID, List[(Info, List[VcfValue])])
  type Input = BufferedIterator[(VariantID, List[(Info, List[VcfValue])])]
  
  
  def parsePatch(file: File): Either[String, Descriptor] = {
    val p = new AnnovarPatchParsers {}
    val rdr = new FileReader(file)
    val result = p.parseAll(p.patch, rdr)
    rdr.close()
    
    result match {
      case p.Success(result, _) => Right(result)
      case error: p.NoSuccess => Left(error.toString)
    }
  }
}


case class AnnovarPatch(in: InputStream, descriptor: AnnovarPatch.Descriptor) extends Patch[BufferedIterator[(VariantID, List[(Info, List[VcfValue])])]] {
  import AnnovarPatch._
  
  def init(vcfInfo: VcfInfo) = {
    val reader = new BufferedReader(new InputStreamReader(in))
    val parsers = new AnnovarTsvParsers {
      val desc = descriptor
    }
    val parser = parsers.annovarRow
    
    val it = Iterator continually (reader.readLine()) takeWhile (_ != null) map (parsers.parse(parser, _)) map {
      case parsers.Success(result, _) =>
        result
        
      case error: parsers.NoSuccess => 
        throw VcfParseException(error.toString)
    }
    
    (it.buffered, VcfInfo(vcfInfo.metadata ++ descriptor.extraInfoCol.keys.toList, vcfInfo.samples))
  }
  
  
  def patch(a: Input, row: VcfRow): (Input, VcfRow) = if (!a.hasNext) {
    
    (a, row)
    
  } else (a.head, row) match {
    case ((VariantID(chrom, pos, ref, alts), infos), (v, fmts, values)) =>
      
      if (v.chromosome == chrom && v.position == pos && v.reference == ref && v.alternates == alts) {
        
        a.next()
        (a, (v.copy(info = v.info ++ infos), fmts, values))
        
      } else {
        
        (a, row)
        
      }
  }
}

// 

case class VariantID(chrom: Variant.Chromosome, pos: Int, ref: String, alts: List[Variant.Alternate])
