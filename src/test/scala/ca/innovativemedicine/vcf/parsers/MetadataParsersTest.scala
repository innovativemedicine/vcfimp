package ca.innovativemedicine.vcf.parsers


import org.scalatest.FunSuite
import ca.innovativemedicine.vcf
import ca.innovativemedicine.vcf._


class MetadataParsersTest extends FunSuite {
  import Metadata._
  
  val samples = "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\tHG00096\tHG00097\tHG00099"
    
  val expectedSamples = List(Sample(VcfId("HG00096")), Sample(VcfId("HG00097")), Sample(VcfId("HG00099")))
  def parser = new MetadataParsers { }

  test("full samples header is parsed correctly") {
    val p = parser
    
    assert((p.parse(p.header, samples) match {
      case p.Success(res, _) => res
      case x => x
    }) === Left(expectedSamples))
    
    assert((p.parse(p.samples, samples) match {
      case p.Success(res, _) => res
      case x => x
    }) === expectedSamples)
  }
  
  test("missing fixed field fails parser") {
    val missing = "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO"
    val p = parser
    assert(!p.parse(p.samples, missing).successful)
  }
  
  test("misspelt fixed field fails parser") {
    val mispelt = "#CHROM\tPXS\tID\tRXF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\tHG00096\tHG00097\tHG00099"
    val p = parser
    assert(!p.parse(p.samples, mispelt).successful)
  }
  
  test("fileformat is an arbitrary string") {
    val p = parser
    assert(p.parse(p.version, "fileformat=VCFv4.1").getOrElse(null) === Version("VCFv4.1"))
    assert(p.parse(p.version, "fileformat=asdfqsdf").getOrElse(null) === Version("asdfqsdf"))
  }
}