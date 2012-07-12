package ca.innovativemedicine.vcf.format

import ca.innovativemedicine.vcf._

import java.io.{ OutputStream, PrintWriter }


object VcfWriter {
  import Metadata._
  import VcfFormatter._

  /**
   * This writes a VCF file to `out`, but first applies a set of patches (in
   * order of their appearance in the params).
   */
  def writeWithPatch(out: OutputStream, p: Patch[_], ps: Patch[_]*)(vcfInfo: VcfInfo, rows: Iterator[VcfRow]) {
    (write(out) _).tupled (ps.foldLeft(p.apply _)(_ andThen _)(vcfInfo -> rows))
  }
  
  /**
   * Write a VCF file to `out`.
   */
  def write(out: OutputStream)(vcfInfo: VcfInfo, rows: Iterator[VcfRow]) {
    
    val writer = new PrintWriter(out)
    
    // Write metdata.
    
    vcfInfo.metadata foreach { md =>
      writer.println(formatMetadata(md))
    }
    
    // Write header.
    
    writer.println((formatHeader :: (vcfInfo.samples map (_.id.id))) mkString "\t")
    
    // Write variant calls.
    
    rows foreach { case (variant, fmts, values) =>
      val fixed = List(
          formatChromosome(variant.chromosome),
          variant.position.toString,
          variant.ids mkString ",",
          variant.reference,
          variant.alternates map formatAlternate mkString ",",
          variant.filter map formatFilterResult getOrElse ".",
          variant.info map { case (info, vals) =>
            info.id.id + "=" + (vals map formatVcfValue mkString ",")
          } mkString ";",
          fmts map (_.id.id) mkString ":"
        ) mkString "\t"
      val samples = values map (_ map (_ map formatVcfValue mkString ",") mkString ":")
      
      writer.println((fixed :: samples) mkString "\t")
    }
    
    writer.close()
  }
}