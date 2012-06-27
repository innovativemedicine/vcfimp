package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._
import java.io._
import java.util.zip.GZIPInputStream
import scala.annotation.tailrec


case class VcfParseException(msg: String) extends Exception(msg)


trait VcfParser {
  
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
  
  
  /**
   * Parses the VCF file `file` and pass the metadata and an `Iterator` of
   * variants and genotype data to `f`. The iterator is only valid within `f`.
   */
  def parse[A](file: File, skipErrors: Boolean = true)(f: (VcfInfo, Iterator[Variant]) => A) = {
    val underlying = new FileInputStream(file)
    try {
    	val in = new BufferedReader(new InputStreamReader(maybeDecompress(underlying)), VcfParser.RowBufferSize)
    	val lines = Iterator continually (in.readLine()) takeWhile (_ != null)
    	
    	val mdParser = new MetadataParsers { }
    	
    	@tailrec
    	def parseMetadata(metadata: List[Metadata], it: Iterator[String]): (List[Sample], List[Metadata]) =
    	  mdParser.parseAll(mdParser.header, it.next()) match {
    	    case mdParser.Success(Right(md), _) =>
    	      parseMetadata(md :: metadata, it)
    	      
    	    case mdParser.Success(Left(samples), _) =>
    	      (samples, metadata)
    	      
    	    case error: mdParser.NoSuccess if skipErrors =>
    	      println("Failed to parse metadata row, skipping:\n%s" format error.toString())		// TODO: LOG!!!
    	      parseMetadata(metadata, it)
    	    
    	    case error: mdParser.NoSuccess =>
    	      throw VcfParseException(error.toString())
    	  }
    	
    	val (samples, metadata) = parseMetadata(Nil, lines)
    	val vcf = VcfInfo(metadata, samples)
    	val variantParser = new VariantParsers {
    	  val vcfInfo = vcf
    	}
    	
    	val it = lines flatMap { row =>
    	  variantParser.parse(variantParser.variant, row) match {
    	    case variantParser.Success(variant, _) =>
    	      Some(variant)
    	      
    	    case error: variantParser.NoSuccess if skipErrors =>
    	      println("Failed to parse variant, skipping row:\n%s" format error.toString())
    	      None
    	      
    	    case error: variantParser.NoSuccess =>
    	      throw VcfParseException(error.toString()) 
    	  }
    	}
    	
    	f(vcf, it)
    	
    } finally {
      underlying.close()
    }
  }
}

object VcfParser {
  val RowBufferSize = 1 * 1000 * 1000
}