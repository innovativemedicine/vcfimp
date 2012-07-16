package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._
import java.util.{ concurrent => juc }
import scala.actors.Future
import scala.actors.Actor
import scala.actors.Futures


/**
 * This will parse a VCF file concurrently. One thread is always dedicated to
 * reading in the file and there will also be several workers spun up (actors)
 * to parse the file.
 */
final class ConcurrentVcfParser(vcf: VcfInfo, rows: Iterator[String], batchSize: Int = 100, workers: Int = 4)
extends Runnable with Iterator[Either[String, VcfRow]] {
  require(workers > 0)
  
  import ConcurrentVcfParser._
  
  case object EOF
  
  @volatile private var dead = false
  @volatile private var _stayAlive = true
  private def stayAlive = _stayAlive || Thread.currentThread().isInterrupted()
  
  // Need to wait for parsers and this very thread to die.
  def kill() {
    _stayAlive = false
    
    while (!dead) {
      try {
        Thread.sleep(10)
      } catch {
        case ie: InterruptedException =>
          Thread.currentThread().interrupt()
      }
    }
  }
  
  private val queue = new juc.ArrayBlockingQueue[Future[Any]](batchSize)
  
  // Iterator functions. We let InterruptedExceptions fall through. Is this OK?
  
  private var _head: Future[Any] = null
  private def head: Any = {
    if (_head == null) {
      _head = queue.take()
    }
    _head()
  }
  
  def hasNext = head != EOF
  
  def next = {
    val result = head match {
      case ParsedRow(row) =>
        Right(row)
        
      case FailedRow(msg) =>
        Left(msg)
        
      case EOF =>
        throw new java.util.NoSuchElementException("next() called on empty iterator")
        
      case obj =>
        Left("Expected ParsedRow or FailedRow, but got: " + obj.toString)
    }
    _head = queue.take()
    result
  }
  
  
  // A bunch of actors that'll parse rows for us. 
  private val parsers = Array.fill(workers)(RowParser(vcf: VcfInfo))
  private var _parser = -1
  
  // We use a round-robin approach to routing.
  private def parser: Actor = {
    _parser = (_parser + 1) % parsers.size
    parsers(_parser)
  }
  
  def run() {
    try {
      while (stayAlive && rows.hasNext) {
        val result = parser !! rows.next()
        
        while (stayAlive && !queue.offer(result, queuePollTimeout._1, queuePollTimeout._2)) {
          // Can't be reading these rows too fast, yo.
        }
      }
      
      if (!rows.hasNext) {
        val eof = Futures.future { EOF }
        while (stayAlive && !queue.offer(eof, queuePollTimeout._1, queuePollTimeout._2)) {
          // Spin.
        }
      }
    } finally {
      
      // Kill the parsers and _wait_ for them to finish.
      
      parsers map { _ !! Kill } foreach { _() }
      dead = true
    }
  }
}


object ConcurrentVcfParser {
  
  import Actor._
  import VcfParser._
  
  val queuePollTimeout = (10, juc.TimeUnit.MILLISECONDS)
  
  
  // A `RowParser` can accept a `String` or a `Kill` signal.
  case object Kill
  case object Dead
  
  // A `RowParser` replies with either a `ParsedRow` or a `FailedRow`.
  case class ParsedRow(row: VcfRow)
  case class FailedRow(msg: String)
  
  
  /**
   * An `Actor` that will parse rows from a VCF file.
   */
  def RowParser(vcf: VcfInfo): Actor = actor {
    val parser = new DataParsers { val vcfInfo = vcf }
    
    // Bind each parser to a thread, as we expect them to be kept busy busy.
    while (true) {
      receive {
        case Kill =>
          sender ! Dead
          exit()
          
        case line: String =>
          parser.parse(parser.row, line) match {
            case parser.Success(row, _) =>
              sender ! ParsedRow(row)
              
            case error: parser.NoSuccess =>
              sender ! FailedRow(error.toString)
          }
      }
    }
  }
}