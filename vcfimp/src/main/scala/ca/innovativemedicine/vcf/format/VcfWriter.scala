package ca.innovativemedicine.vcf.format

import ca.innovativemedicine.vcf._

import java.io.{ OutputStream, PrintWriter }

import java.util.{ concurrent => juc }


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
    
    val dataWriter = new ConcurrentVcfDataWriter(writer)
    val t = new Thread(dataWriter)
    t.start()
    
    // Any patches will be applied in this thread, the row is written in thread t.
    
    rows foreach { dataWriter write _ }
    
    dataWriter.finish()
    t.join()
    
    writer.close()
  }
}


object ConcurrentVcfDataWriter {
  case object EOF
  case class WriteRow(row: VcfRow)
}


class ConcurrentVcfDataWriter(writer: PrintWriter) extends Runnable {
  import Metadata._
  import VcfFormatter._
  import ConcurrentVcfDataWriter._
  
  @volatile private var _stayAlive = true
  def kill() { _stayAlive = false }
  private def stayAlive = _stayAlive || Thread.currentThread().isInterrupted()
  
  private val queue: juc.BlockingQueue[Any] = new juc.ArrayBlockingQueue[Any](100)
  
  def write(row: VcfRow) {
    queue.put(WriteRow(row))
  }
  
  def finish() {
    queue.put(EOF)
  }
  
  def run() {
    while (stayAlive) {
      Option(queue.poll(10, juc.TimeUnit.MILLISECONDS)) match {
        case Some(WriteRow((variant, fmts, values))) =>
          val fixed = List(
              formatChromosome(variant.chromosome),
              variant.position.toString,
              variant.ids mkString ",",
              variant.reference,
              variant.alternates map formatAlternate mkString ",",
              variant.quality map (_.toString) getOrElse ".",
              variant.filter map formatFilterResult getOrElse ".",
              variant.info map { case (info, vals) =>
                info.id.id + "=" + (vals map formatVcfValue mkString ",")
              } mkString ";",
              fmts map (_.id.id) mkString ":"
            ) mkString "\t"
          val samples = values map (_ map (_ map formatVcfValue mkString ",") mkString ":")
          
          writer.println((fixed :: samples) mkString "\t")
          
        case Some(EOF) =>
          _stayAlive = false
        
        case Some(_) | None =>
      }
    }
  }
}