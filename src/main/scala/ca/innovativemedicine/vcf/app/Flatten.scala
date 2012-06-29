package ca.innovativemedicine.vcf.app

import ca.innovativemedicine.vcf._
import ca.innovativemedicine.vcf.parsers._
import ca.innovativemedicine.vcf.format.VcfFormatter

import java.io.{ File, FileOutputStream, PrintWriter, InputStream }


case class FlattenParams(
    vcfFile: Either[InputStream, File] = Left(System.in),
    prefix: Option[String] = None,
    pattern: String = "%p-%s-%i.tsv",
    info: Option[List[String]] = None,
    genotype: Option[List[String]] = None,
    skipHeader: Boolean = false,
    ignoreErrors: Boolean = false
    ) {
  
  /** Returns true if these params are only valid for outputting a single file. */
  def singleFile: Boolean = {
    val reduced = pattern.replace("%%", "x")
    !((reduced contains "%s") || (reduced contains "%i"))
  }
  
  lazy val preferredPrefix: String = (vcfFile, prefix) match {
    case (_, Some(prefix)) => prefix
    case (Right(file), _) => file.getName()
    case (Left(_), _) => "flat-vcf"
  }
  
  def getOutFile(s: String, i: Int): String = {
    "%[spi%]".r.replaceSomeIn(pattern, _.matched match {
      case "%%" => Some("%")
      case "%s" => Some(s)
      case "%p" => Some(preferredPrefix)
      case "%i" => Some(i.toString)
      case _ => None
    })
  }
}


/**
 * This application is used to "flatten" a VCF file. That is, it expands the
 * INFO fields and the sample data to their own individual columns. To deal
 * with multiple samples, it can create a new, separate, TSV file for each
 * sample.
 */
object Flatten extends App {
  import Metadata._
  
  // Parse args, flatten VCF file, log error messages, and exit with appropriate error code.
  
  parseArgs(FlattenParams(), args.toList) map { params =>

    params.vcfFile match {
      case Right(file) =>
        VcfParser().parse(file, params.ignoreErrors)(flatten(params))
        
      case Left(in) =>
        VcfParser().parse(in, params.ignoreErrors)(flatten(params))
    }
    
  } map {
    case Right(_) => 0
    case Left(msg) =>
      Console.err.println(msg)
      2    
  } orElse Some(1) foreach {
    System.exit(_)
  }
 
  
  def usageString = """
Usage: vcflatten [options] <filename>

    -i <keys> | --info <keys>
        Specify a semicolon-separated list of INFO IDs to output for each variant from the VCF file.
    
    -g <keys> | --genotype <keys>
        Specify a colon-separated list of FORMAT IDs to output for each sample from the VCF file.
    
    --no-header
        If this flag is set, the TSV header won't be written to any of the output files. 
    
    --ignore-errors
        If this flag is set, then any errors in the VCF file will be ignored, and the invalid rows will be skipped.
    
    --prefix <filename prefix>
        A filename prefix that can be used in the output pattern. If this is not set, then the prefix is the same as <filename>.
    
    -o <pattern> | --pattern <pattern>
        The pattern to use when generating output files. The default is "%p-%s-%d". Valid special patterns are:
    
          %p    Include the "prefix" here (either <filename> or given in --prefix <prefix>
          %s    The name of the sample, taken from the header of the VCF file.
          %i    The index of the sample (1-based).
          %%    A single, literal '%'.
    
        If neither %s nor %d is provided, then the VCF file must have only 1 sample.
"""
    
  def usage() {
    println(usageString)
  }
  
  
  def parseArgs(params: FlattenParams, args: List[String]): Option[FlattenParams] = args match {
    case Nil =>
      Some(params)
      
    case ("-h" | "--help") :: args =>
      usage()
      None
      
    case ("-i" | "--info") :: keys :: args =>
      if (keys == "-") parseArgs(params.copy(info = Some(Nil)), args) else {
        parseArgs(params.copy(info = Some(keys.split(":").toList)), args)
      }
    
    case ("-g" | "--genotype") :: keys :: args =>
      if (keys == "-") parseArgs(params.copy(genotype = Some(Nil)), args) else {
        parseArgs(params.copy(genotype = Some(keys.split(";").toList)), args)
      }
      
    case "--no-header" :: args =>
      parseArgs(params.copy(skipHeader = true), args)
      
    case "--prefix" :: prefix :: args =>
      parseArgs(params.copy(prefix = Some(prefix)), args)
      
    case ("-o" | "--pattern") :: pattern :: args =>
      parseArgs(params.copy(pattern = pattern), args)
      
    case "--ignore-errors" :: args =>
      parseArgs(params.copy(ignoreErrors = true), args)
      
    case fn :: args =>
      parseArgs(params.copy(vcfFile = Right(new File(fn))), args)
  }
  
  
  /**
   * Returns a `Reader` for a given set of parameters that will output a set of
   * flattened files.
   */
  def flatten(params: FlattenParams): VcfParser.Reader[Either[String, Unit]] = (vcfInfo, it) => {
    
    if (params.singleFile && vcfInfo.samples.size > 1) {
      Left("VCF file contains %d samples, but output filename pattern ('%s') can only be used with 1 sample VCF file." format
          (vcfInfo.samples.size, params.pattern))
          
    } else {
      
      // Each sample is given its own file, suffixed with its 1-based index.
      
      val files = vcfInfo.samples.zipWithIndex map { case (s, i) =>
        new File(params.getOutFile(s.id.id, i))
      }
      val outs = (files map (new PrintWriter(_))).toIndexedSeq
      
      
      // Get a list of INFO & FORMAT fields to print out for each variant.
      
      
      val infos: Seq[Info] = params.info map {
        _ flatMap { i => vcfInfo.getTypedMetadata[Info](VcfId(i)) } 
      } getOrElse (vcfInfo.metadata.collect { case f: Metadata.Info => f }).sortBy(_.id.id)
      
      val formats: Seq[Format] = params.genotype map {
        _ flatMap { f => vcfInfo.getTypedMetadata[Format](VcfId(f)) }
      } getOrElse (vcfInfo.metadata.collect { case f: Metadata.Format => f }).sortBy(_.id.id)
      
      
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
      
      Right(())
    }
  }
}