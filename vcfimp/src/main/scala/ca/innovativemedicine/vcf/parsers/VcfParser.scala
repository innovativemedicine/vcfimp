package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._
import java.io._
import java.util.zip.GZIPInputStream
import scala.collection.mutable
import scala.annotation.tailrec
import java.util.{ concurrent => juc }
import java.util.concurrent.{ TimeUnit, BlockingQueue }
import scala.actors.Actor


case class VcfParseException(msg: String) extends Exception(msg)


trait VcfParser {
  import VcfParser._
  import Metadata._
  
  
  /**
   * Determines if `in` is gzipped and, if so, wraps it in a `GZIPInputStream`
   * and returns it. Otherwise this returns an `InputStream` that can read the
   * original one. Do NOT attempt to re-use `in` after calling this method.
   * Doing so will cause fire and brimstone to rain down upon your cubicle.
   * 
   * @param in An `InputStream` that may need to be decompressed.
   * @returns A new `InputStream` to read an uncompressed VCF from.
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
  	      (samples, metadata.reverse)
  	      
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
      val lines = vcfRows(underlying)
  	  f(lines)
  	  
    } finally {
      underlying.close()
    }
  }
  
  
  protected def withFile[A](file: File)(f: InputStream => A): A = {
    val in = new FileInputStream(file)
    try {
      f(in)
    } finally {
      in.close()
    }
  }
  
  
  /** Returns an `Iterator` of the rows of `underlying` efficiently. */
  protected def vcfRows(underlying: InputStream) = {
    val in = new BufferedReader(new InputStreamReader(maybeDecompress(underlying)), VcfParser.RowBufferSize)
    Iterator continually (in.readLine()) takeWhile (_ != null)
  }
  
  
  /**
   * Returns the metadata for a VCF file.
   */
  def parseMetadata(file: File): VcfInfo = withVcfFile(file)(parseMetadata(_, false))
  
  
  /**
   * Parses the VCF file `file` and pass the metadata and an `Iterator` of
   * variants and genotype data to `f`. The iterator is only valid within `f`.
   */
  def parseFile[A](file: File, skipErrors: Boolean, workers: Int = 0)(f: Reader[A]): A =
    withFile(file)(parse(_, skipErrors, workers)(f))
  
    
  /**
   * Parses a VCF file from an arbitrary `InputStream`.
   */
  def parse[A](in: InputStream, skipErrors: Boolean, workers: Int = 0)(f: Reader[A]): A =
    parse(vcfRows(in), skipErrors, workers)(f)
 
  
  protected def parse[A](lines: Iterator[String], skipErrors: Boolean, workers: Int)(f: Reader[A]): A = {
    require(workers >= 0)

  	val vcf = parseMetadata(lines, skipErrors)
  	
  	vcf.version map (_.toUpperCase) match {
  	  case Some(ValidVersion(ver)) =>
  	  case Some(ver) if skipErrors =>
  	    // TODO: Log this as a warning.
  	  case Some(ver) =>
  	    throw VcfParseException("Expected to find a VCF4.0 or VCF4.1 file, but found %s." format ver)
  	  case None =>
  	    // TODO: Log this as a warning.
  	}
  	
  	if (workers > 0) {
    	val parser = new ConcurrentVcfParser(vcf, lines, workers = workers)
    	new Thread(parser).start()
    	
    	val result = f(vcf, parser flatMap {
    	  case Right(row) =>
    	    Some(row)
    	    
    	  case Left(error) if skipErrors =>
    	    System.err.println("Failed to parse variant, skipping row:\n%s" format error.toString())
          None
          
        case Left(error) =>
          throw VcfParseException(error.toString())
    	})
    	parser.kill()
    	result
  	} else {
  	  val parser = new DataParsers { val vcfInfo = vcf }
  	  
  	  f(vcf, lines flatMap { row =>
    	  parser.parse(parser.row, row) match {
    	    case parser.Success(res, _) =>
    	      Some(res)
    	      
    	    case error: parser.NoSuccess if skipErrors =>
    	      println("Failed to parse variant, skipping row:\n%s" format error.toString())
    	      None
    	      
    	    case error: parser.NoSuccess =>
    	      throw VcfParseException(error.toString())
    	  }
    	})
  	}
  }
}


object VcfParser {
  import Metadata._
  
  val RowBufferSize = 1 * 1000 * 1000
  
  val ValidVersion = """([vV][cC][fF][vV]?4\.[01])"""r
  
  type Reader[A] = (VcfInfo, Iterator[(Variant, List[Format], List[List[List[VcfValue]]])]) => A
  
  def apply(): VcfParser = new VcfParser { }
}