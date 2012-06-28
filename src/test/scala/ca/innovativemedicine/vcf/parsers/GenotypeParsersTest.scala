package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._

import org.scalatest.FunSuite


class GenotypeParsersTest extends FunSuite {
  import Metadata._
  
  val formats = List(
      Format(VcfId("GT"), Arity.Exact(1), Type.StringType, None),
      Format(VcfId("DS"), Arity.Exact(1), Type.FloatType, None),
      Format(VcfId("GL"), Arity.Variable, Type.FloatType, None)
    )
    
  val s1t = "0|0:0.000:-0.04,-1.07,-5.00"
  val s1 = List(List(VcfString("0|0")), List(VcfFloat(0.0)), List(VcfFloat(-0.04), VcfFloat(-1.07), VcfFloat(-5)))
  
  val s2t = "0|1:0.000:-0.02,-1.34,-5.00"
  val s2 = List(List(VcfString("0|1")), List(VcfFloat(0.0)), List(VcfFloat(-0.02), VcfFloat(-1.34), VcfFloat(-5)))
    
  val s3t = "1/1:0.000:-0.11,-0.67,-4.40"
  val s3 = List(List(VcfString("1/1")), List(VcfFloat(0.0)), List(VcfFloat(-0.11), VcfFloat(-0.67), VcfFloat(-4.40)))
  
  val row = "GT:DS:GL\t%s\t%s\t%s" format (s1t, s2t, s3t)
  
    
  val vcf = VcfInfo(formats, List(Sample(VcfId("a")), Sample(VcfId("b")), Sample(VcfId("c"))))
  
  
  lazy val parser = new GenotypeParsers {
    val vcfInfo = vcf
  }
  
  test("empty FORMATs are valid") {
    assert(parser.parse(parser.format, "").get === Nil)
  }
      
  test("FORMATs are parsed correctly and in order") {
    assert(parser.parse(parser.format, "GT:DS:GL").get === formats)
    assert(parser.parse(parser.format, "GL:GT:DS").get === (formats.last :: formats.init))
    assert(parser.parse(parser.format, "DS:GL").get === formats.tail)
  }
  
  test("non-existant FORMATs cause the parser to fail") {
    assert(!parser.parse(parser.format, "ZZ:TOP").successful)
  }
  
  test("valid sample data is parsable given FORMATs") {
    assert(parser.parseAll(parser.genotype(formats, 3, 2), s1t).get === s1)
    assert(parser.parseAll(parser.genotype(formats, 3, 2), s2t).get === s2)
    assert(parser.parseAll(parser.genotype(formats, 3, 2), s3t).get === s3)
  }
  
  test("sample data missing gt data fails to parse") {
    assert(!parser.parseAll(parser.genotype(formats, 3, 2), "0|0:0.000").successful)
  }
  
  test("valid full-row, with formats and sample data, can be parsed") {
    assert(parser.parseAll(parser.genotypes(3, 2), row).get === (formats, List(s1, s2, s3)))
  }
  
  test("row missing a sample should fail") {
    assert(!parser.parseAll(parser.genotypes(4, 2), row).successful)
  }
}