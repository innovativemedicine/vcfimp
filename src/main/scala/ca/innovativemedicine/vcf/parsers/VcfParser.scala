package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._
import java.io._
import java.util.zip.GZIPInputStream
import scala.annotation.tailrec


case class VcfParseException(msg: String) extends Exception(msg)


trait VcfParser {
  import Metadata._
  
  
  /**
   * Determines if `in` is gzipped and, if so, wraps it in a `GZIPInputStream`
   * and returns it. Otherwise this just returns `in`.
   * 
   * @param in An `InputStream` that may need to be decompressed.
   */
  private def maybeDecompress(in: InputStream): InputStream = {
    val pb = new PushbackInputStream(in, 2)
    val b1 = pb.read()
    val b2 = pb.read()
    pb.unread(b2)
    pb.unread(b1)
    
    if (((b2 << 8) | b1) == GZIPInputStream.GZIP_MAGIC) {
      new GZIPInputStream(pb)
    } else {
      pb
    }
  }
  
  
  protected def parseMetadata(rows: Iterator[String], skipErrors: Boolean): VcfInfo = {
  	val parser = new MetadataParsers { }
  	
  	@tailrec
  	def parseMetadata(metadata: List[Metadata], it: Iterator[String]): (List[Sample], List[Metadata]) =
  	  parser.parseAll(parser.header, it.next()) match {
  	    case parser.Success(Right(md), _) =>
  	      parseMetadata(md :: metadata, it)
  	      
  	    case parser.Success(Left(samples), _) =>
  	      (samples, metadata)
  	      
  	    case error: parser.NoSuccess if skipErrors =>
  	      println("Failed to parse metadata row, skipping:\n%s" format error.toString())		// TODO: LOG!!!
  	      parseMetadata(metadata, it)
  	    
  	    case error: parser.NoSuccess =>
  	      throw VcfParseException(error.toString())
  	  }
  	
  	val (samples, metadata) = parseMetadata(Nil, rows)
  	VcfInfo(metadata, samples)
  }
  
  
  /** Gives `f` an `Iterator` over the rows of `file`. */
  protected def withVcfFile[A](file: File)(f: Iterator[String] => A): A = {
    val underlying = new FileInputStream(file)
    try {
      val in = new BufferedReader(new InputStreamReader(maybeDecompress(underlying)), VcfParser.RowBufferSize)
  	  val lines = Iterator continually (in.readLine()) takeWhile (_ != null)
  	  f(lines)
  	  
    } finally {
      underlying.close()
    }
  }
  
  
  /**
   * Returns the metadata for a VCF file.
   */
  def parseMetadata(file: File): VcfInfo = withVcfFile(file)(parseMetadata(_, false))
  
  
  /**
   * Parses the VCF file `file` and pass the metadata and an `Iterator` of
   * variants and genotype data to `f`. The iterator is only valid within `f`.
   */
  def parse[A](file: File, skipErrors: Boolean = true)
  		(f: (VcfInfo, Iterator[(Variant, List[Format], List[List[List[VcfValue]]])]) => A) = {
    withVcfFile(file) { lines =>
      
    	val vcf = parseMetadata(lines, skipErrors)
    	val parser = new DataParsers {
    	  val vcfInfo = vcf
    	}
    	
    	val it = lines flatMap { row =>
    	  parser.parse(parser.row, row) match {
    	    case parser.Success(res, _) =>
    	      Some(res)
    	      
    	    case error: parser.NoSuccess if skipErrors =>
    	      println("Failed to parse variant, skipping row:\n%s" format error.toString())
    	      None
    	      
    	    case error: parser.NoSuccess =>
    	      throw VcfParseException(error.toString())
    	  }
    	}
    	
    	f(vcf, it)
    }
  }
}


object VcfParser {
  val RowBufferSize = 1 * 1000 * 1000
  
  def apply(): VcfParser = new VcfParser { }
}