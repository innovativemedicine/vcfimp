package ca.innovativemedicine.vcf.format

import ca.innovativemedicine.vcf._


/**
 * A `Patch[A]` represents a simple transformation of a VCF file. Extra
 * metadata can only be added at the beginning and patches work with only
 * a single row at a time. Some state (of type `A`) is carried through the
 * transformation though. 
 */
trait Patch[A] extends (((VcfInfo, Iterator[VcfRow])) => (VcfInfo, Iterator[VcfRow])) { self =>
  import Metadata._
  
  /**
   * Initializes a `Patcher` given the VCF metadata (`VcfInfo`).
   */
  def init(vcf: VcfInfo): (A, VcfInfo)
  
  /**
   * Patches a single row of a VCF file.
   */
  def patch(a: A, row: VcfRow): (A, VcfRow)
  
  /**
   * Applies this patch to a VCF file.
   */
  def apply(vcf: (VcfInfo, Iterator[VcfRow])): (VcfInfo, Iterator[VcfRow]) = vcf match {
    case (vcfInfo, rows) =>
      val (a, vcf) = init(vcfInfo)
      
      vcf -> (rows.scanLeft(a -> (null: VcfRow)) { case ((a, _), row) =>
        patch(a, row)
      } drop 1 map (_._2))
  }
  
  
  // val asdf: Iterator[State[A, Row]] = rows map (row => state[A, Row](a => patch.patch(a, row)))
  // val blah: State[A, Stream[Row]] = asdf.toStream.sequence[({ type L[a] = State[A, a] })#L, Row]
}


object Patch {
  
  /** Keeps only samples with IDs in `ids`, removes all others. */
  def withSamples(ids: Iterable[String]): Patch[List[Boolean]] = {
    val members = ids.toSet
    
    new Patch[List[Boolean]] {
      def init(vcf: VcfInfo) = {
        val test = vcf.samples map (members contains _.id.id)
        (test, vcf.copy(samples = vcf.samples filter (members contains _.id.id)))
      }
      
      def patch(test: List[Boolean], row: VcfRow) = row match { case (v, fmts, data) =>
        test -> (v, fmts, (data zip test) collect {
          case (data, true) => data
        })
      }
    }
  }
  
  /** Restricts a VCF to only have INFO with IDs in `ids`. */
  def withInfo(ids: Iterable[String]): Patch[Unit] = {
    val members = ids.toSet
    
    Patch { case (v, f, d) =>
      (v.copy(info = v.info filter (members contains _._1.id.id)), f, d)
    }
  }
  
  /** Returns a simple row-mapping patch. */
  def apply(f: VcfRow => VcfRow) = new Patch[Unit] {
    def init(vcf: VcfInfo) = ((), vcf)
    def patch(a: Unit, row: VcfRow) = ((), f(row))
  }
  
  /** Returns a row-mapping patch with state not based on any metadata. */
  def apply[A](a: A, f: (A, VcfRow) => (A, VcfRow)) = new Patch[A] {
    def init(vcf: VcfInfo) = (a, vcf)
    def patch(a: A, row: VcfRow) = f(a, row)
  }
  
  /** A convenient way to construct a `Patch` (vs. inheritance). */
  def apply[A](f: VcfInfo => (A, VcfInfo), g: (A, VcfRow) => (A, VcfRow)) = new Patch[A] {
    def init(vcf: VcfInfo) = f(vcf)
    def patch(a: A, row: VcfRow) = g(a, row)
  }
}