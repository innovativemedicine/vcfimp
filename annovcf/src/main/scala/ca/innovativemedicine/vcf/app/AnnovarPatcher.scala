package ca.innovativemedicine.vcf.app

import ca.innovativemedicine.vcf._
import ca.innovativemedicine.vcf.format._
import ca.innovativemedicine.vcf.parsers._
import ca.innovativemedicine.vcf.annovar._
import java.io.{ File, InputStream, FileInputStream, BufferedInputStream, OutputStream, FileOutputStream, BufferedOutputStream }
import scala.annotation.tailrec


case class AnnovarPatcherParams(
    in: Option[InputStream],
    out: Option[OutputStream],
    patches: List[(AnnovarPatch.Descriptor, File)],
    workers: Int = 0)


object AnnovarPatcher extends App {
  
  parseOptions(args.toList, AnnovarPatcherParams(None, None, Nil)) match {
    case Left(msg) =>
      println(msg)
      sys.exit(1)
    
    case Right(opts) =>
      val vcfParser = VcfParser()
      
      vcfParser.parse(opts.in getOrElse System.in, skipErrors = false, workers = opts.workers) { (vcfInfo, rows) =>
        opts.patches map { case (desc, annovarFile) =>
          AnnovarPatch(new BufferedInputStream(new FileInputStream(annovarFile)), desc)
        } match {
          case p :: ps =>
            VcfWriter.writeWithPatch(opts.out getOrElse System.out, p, ps: _*)(vcfInfo, rows)
            
          case Nil =>
            VcfWriter.write(opts.out getOrElse System.out)(vcfInfo, rows)
        }
      }
  }
  
  
  def parseOptions(args: List[String], params: AnnovarPatcherParams): Either[String, AnnovarPatcherParams] = args match {
    case Nil => Right(params.copy(patches = params.patches.reverse))
    
    case ("-p" | "--patch") :: patchFileName :: annovarFileName :: args =>
      val patchFile = new File(patchFileName)
      val annovarFile = new File(annovarFileName)
      
      if (!patchFile.isFile() || !patchFile.canRead()) {
        Left("Cannot read patch file: %s" format patchFileName)
        
      } else if (!annovarFile.isFile() || !annovarFile.canRead()) {
        Left("Cannot read Annovar TSV file: %s" format annovarFileName)
        
      } else {
        AnnovarPatch.parsePatch(patchFile) match {
          case Left(msg) =>
            Left("Error parsing patch file '%s':\n%s" format (patchFileName, msg))
            
          case Right(desc) =>
            parseOptions(args, params.copy(patches = (desc -> annovarFile) :: params.patches))
        }
      }
      
    case ("-o" | "--output") :: outFileName :: args =>
      if (params.out.isDefined) {
        params.out foreach { _.close() }
        Left("You can only specify at-most 1 output file (--output or -o).")
        
      } else {
        val outFile = new File(outFileName)
        if (outFile.exists && !outFile.canWrite) {
          Left("Cannot write to output file: %s" format outFileName)
          
        } else {
          try {
            parseOptions(args, params.copy(out = Some(new BufferedOutputStream(new FileOutputStream(outFile)))))
            
          } catch {
            case ioe: java.io.IOException =>
              Left("Couldn't open %s for writing:\nioe.getMessage()" format outFileName)
          }
        }
      }
      
    case "--workers" :: count :: args =>
      try {
        val workers = count.toInt
        if (workers < 0)
          throw new NumberFormatException()
        parseOptions(args, params.copy(workers = workers))
      } catch {
        case nfe: NumberFormatException =>
          Left("--workers only accepts a positive integer")
      }
      
    case inFileName :: args =>
      if (params.in.isDefined) {
        params.in foreach { _.close() }
        Left("You can only specify at-most 1 input file.")
        
      } else {
        val inFile = new File(inFileName)
        if (!inFile.exists || !inFile.canRead) {
          Left("Cannot read from file '%s'." format inFileName)
          
        } else {
          try {
            parseOptions(args, params.copy(in = Some(new BufferedInputStream(new FileInputStream(inFile)))))
            
          } catch {
            case ioe: java.io.IOException =>
              Left("Couldn't open %s for reading:\nioe.getMessage()" format inFileName)
          }
        }
      }
  }
}