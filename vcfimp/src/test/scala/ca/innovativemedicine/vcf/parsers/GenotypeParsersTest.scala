package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._

import org.scalatest.FunSuite


class GenotypeParsersTest extends FunSuite {
  import Metadata._
  
  val formats = List(
      Format(VcfId("GT"), Arity.Exact(1), Type.StringType, None),
      Format(VcfId("DS"), Arity.Exact(1), Type.FloatType, None),
      Format(VcfId("GL"), Arity.Variable, Type.FloatType, None),
      Format(VcfId("XX"), Arity.MatchGenotypeCount, Type.IntegerType, None)
    )
    
  val s1t = "0|0:0.000:-0.04,-1.07,-5.00:1,2"
  val s1 = List(List(VcfString("0|0")), List(VcfFloat(0.0)), List(VcfFloat(-0.04), VcfFloat(-1.07), VcfFloat(-5)), List(VcfInteger(1), VcfInteger(2)))
  
  val s2t = "0|1:0.000:-0.02,-1.34,-5.00:3,4"
  val s2 = List(List(VcfString("0|1")), List(VcfFloat(0.0)), List(VcfFloat(-0.02), VcfFloat(-1.34), VcfFloat(-5)), List(VcfInteger(3), VcfInteger(4)))
    
  val s3t = "1/1/1:0.000:-0.11,-0.67,-4.40:5,6,7"
  val s3 = List(List(VcfString("1/1/1")), List(VcfFloat(0.0)), List(VcfFloat(-0.11), VcfFloat(-0.67), VcfFloat(-4.40)), List(VcfInteger(5), VcfInteger(6), VcfInteger(7)))
  
  val s4t = "0|1|0:.:.:.,.,."
  val s4 = List(List(VcfString("0|1|0")), List(VcfMissing), List(VcfMissing), List(VcfMissing, VcfMissing, VcfMissing))
  
  val row = "GT:DS:GL:XX\t%s\t%s\t%s" format (s1t, s2t, s3t)
  val rowWoSample = "GT:DS:GL:XX\t%s\t%s" format (s1t, s2t)
  
    
  val vcf = VcfInfo(formats, List(Sample(VcfId("a")), Sample(VcfId("b")), Sample(VcfId("c"))))
  
  
  lazy val parser = new GenotypeParsers {
    val vcfInfo = vcf
  }
  
  test("empty FORMATs are valid") {
    assert(parser.parse(parser.format, "").get === Nil)
  }
      
  test("FORMATs are parsed correctly and in order") {
    assert(parser.parse(parser.format, "GT:DS:GL:XX").get === formats)
    assert(parser.parse(parser.format, "XX:GT:DS:GL").get === (formats.last :: formats.init))
    assert(parser.parse(parser.format, "DS:GL").get === formats.tail.init)
  }
  
  test("non-existant FORMATs cause the parser to fail") {
    assert(!parser.parse(parser.format, "ZZ:TOP").successful)
  }
  
  test("valid sample data is parsable given FORMATs") {
    assert(parser.parseAll(parser.genotype(formats, 2), s1t).get === s1)
    assert(parser.parseAll(parser.genotype(formats, 2), s2t).get === s2)
    assert(parser.parseAll(parser.genotype(formats, 2), s3t).get === s3)
  }
  
  test("samples with missing values noted parse correctly") {
    assert(parser.parseAll(parser.genotype(formats, 2), s4t).get === s4)
  }
  
  test("sample data missing gt data fails to parse") {
    assert(!parser.parseAll(parser.genotype(formats, 2), "0|0:0.000").successful)
  }
  
  test("valid full-row, with formats and sample data, can be parsed") {
    assert(parser.parseAll(parser.genotypes(2), row).get === (formats, List(s1, s2, s3)))
  }
  
  test("row missing a sample should fail") {
    assert(!parser.parseAll(parser.genotypes(2), rowWoSample).successful)
  }
  
  test("data with arity G parses correct number of values") {
    assert(!parser.parseAll(parser.genotype(formats, 2), s1t + ",3").successful)
    assert(!parser.parseAll(parser.genotype(formats, 2), s1t dropRight 2).successful)
  }
}
