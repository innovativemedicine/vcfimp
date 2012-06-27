package ca.innovativemedicine.vcf.app

import ca.innovativemedicine.vcf._
import ca.innovativemedicine.vcf.parsers._
import scopt.immutable._
import java.io.File
import java.io.FileOutputStream
import java.io.PrintWriter
import format.VcfFormatter
import java.io.InputStream


case class FlattenParams(
    vcfFile: Either[InputStream, File] = Left(System.in),
    prefix: Option[String] = None,
    info: Option[List[String]] = None,
    genotype: Option[List[String]] = None,
    skipHeader: Boolean = false,
    ignoreErrors: Boolean = false
    ) {
  
  def getOutFile(i: Int): String = (vcfFile, prefix) match {
    case (_, Some(prefix)) => prefix + i
    case (Right(file), _) => file.getCanonicalPath() + "-" + i
    case (Left(_), _) => "flat-vcf-" + i
  }
}
    

final class FlattenOps(
    programName: Option[String],
    version: Option[String] = None,
    errorOnUnknownArgument: Boolean = true
    ) extends OptionParser[FlattenParams](programName, version, errorOnUnknownArgument) {
  
  def options = Seq(
      
      opt("i", "info", "semicolon-separated list of INFO keys to output (- for none)") {
        case ("-", params) => params.copy(info = Some(Nil))
        case (keys, params) => params.copy(info = Some(keys.split(";").toList))
      },
      
      opt("g", "genotype", "colon-separated list of FORMAT keys to output (- for none)") {
        case ("-", params) => params.copy(genotype = Some(Nil))
        case (keys, params) => params.copy(genotype = Some(keys.split(":").toList))
      },
      
      booleanOpt("n", "no-header", "omit VCF-like header from output") { (skip, params) =>
        params.copy(skipHeader = skip)
      },
      
      opt("o", "prefix", "output file prefix") { (prefix, params) =>
        params.copy(prefix = Some(prefix))
      },
      
      booleanOpt("s", "ignore-errors", "continue flattening VCF after error") { (skip, params) =>
        params.copy(ignoreErrors = skip)
      },
      
      arg("<VCF file>", "a VCF file to flatten") { (fn, params) =>
        params.copy(vcfFile = Right(new File(fn)))
      }
    )
}
    

object Flatten extends App {
  import Metadata._
  
  val optsParser = new FlattenOps(Some("vcflatten"))
  
  optsParser.parse(args, FlattenParams()) map { params =>
    val parser = VcfParser()
    
    params.vcfFile match {
      case Right(file) =>
        parser.parse(file, params.ignoreErrors)(flatten(params))
        
      case Left(in) =>
        parser.parse(in, params.ignoreErrors)(flatten(params))
    }
  }
  
  def flatten(params: FlattenParams): VcfParser.Reader[Unit] = (vcfInfo, it) => {
    
    // Each sample is given its own file, suffixed with its 1-based index.
    
    val files = (1 to vcfInfo.samples.size) map { i => new File(params getOutFile i) }
    val outs = (files map (new PrintWriter(_))).toIndexedSeq
    
    
    // Get a list of INFO & FORMAT fields to print out for each variant.
    
    
    val infos: Seq[Info] = params.info map {
      _ flatMap { i => vcfInfo.getTypedMetadata[Info](VcfId(i)) } 
    } getOrElse vcfInfo.getInfoFields.sortBy(_.id.id)
    
    val formats: Seq[Format] = params.genotype map {
      _ flatMap { f => vcfInfo.getTypedMetadata[Format](VcfId(f)) }
    } getOrElse vcfInfo.getFormatFields.sortBy(_.id.id)
    
    
    // Write out header, unless told not to.
    
    
    if (!params.skipHeader) {
      val header = List("#CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER") ++
        (infos map (_.id.id)) ++ (formats map (_.id.id))
      
      outs foreach { _.println(header mkString "\t") }
    }
    
    
    // Write out the main body of the TSV.
    
    
    it foreach { case (v, fmts, samples) =>
      
      // Variant/fixed columns. These are always printed.
      
      val fixedcols = List(
          VcfFormatter.formatChromosome(v.chromosome),
          v.position.toString,
          v.ids mkString ";",
          v.reference,
          v.alternates map (VcfFormatter formatAlternate _) mkString ",",
          v.quality map (_.toString) getOrElse ".",
          v.filter map (VcfFormatter formatFilterResult _) getOrElse "."
        )
        
      // Requested INFO fields are gathered.
        
      val infocols = infos map (v.info get _ map {
        _ map (VcfFormatter formatVcfValue _) mkString ","
      } getOrElse ".")
      
      val varcols = (fixedcols ++ infocols) mkString "\t"
      
      // Sample specific fields are gathered, appended, and printed.
      
      samples.zipWithIndex foreach { case (s, i) =>
        val gt = (fmts zip s).toMap
        val samplecols = formats.toList map (gt get _ map {
          _ map (VcfFormatter formatVcfValue _) mkString ","
        } getOrElse ".")
        
        outs(i).println((varcols :: samplecols) mkString "\t")
      }
    }
    
    
    // Clean up.
    
    
    outs foreach { _.close() }
  }
}