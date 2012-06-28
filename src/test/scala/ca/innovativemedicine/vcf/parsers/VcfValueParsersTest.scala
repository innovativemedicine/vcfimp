package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._
import org.scalatest.FunSuite


class VcfValueParsersTest extends FunSuite {
  val parser = new VcfValueParsers {}
  
  test("repNsep") {
    assert(parser.parseAll(parser.repNsep(3, parser.vcfInteger, parser.elem(',')), "123,-245,367").successful)
    assert(parser.parseAll(parser.repNsep(1, parser.vcfInteger, parser.elem(',')), "123").successful)
    assert(parser.parseAll(parser.repNsep(2, parser.vcfInteger, parser.elem(',')), "123,-245").successful)
    assert(parser.parseAll(parser.repNsep(0, parser.vcfInteger, parser.elem(',')), "").successful)
    
    assert(!parser.parseAll(parser.repNsep(0, parser.vcfInteger, parser.elem(',')), "1234").successful)
    assert(!parser.parseAll(parser.repNsep(2, parser.vcfInteger, parser.elem(',')), "1234").successful)
  }
  
  test("vcfInteger parses valid integers") {
    assert(parser.parseAll(parser.vcfInteger, "1234").get === VcfInteger(1234))
    assert(parser.parseAll(parser.vcfInteger, "0").get === VcfInteger(0))
    assert(parser.parseAll(parser.vcfInteger, "-1234").get === VcfInteger(-1234))
    
    assert(parser.parseAll(parser.vcfInteger, "01234").get === VcfInteger(1234))
  }
  
  test("vcfFloat parses valid integers") {
    assert(parser.parseAll(parser.vcfFloat, "1234").get === VcfFloat(1234))
    assert(parser.parseAll(parser.vcfFloat, "0.0001234").get === VcfFloat(0.0001234))
    assert(parser.parseAll(parser.vcfFloat, "1234e100").get === VcfFloat(1234e100))
    assert(parser.parseAll(parser.vcfFloat, "-0.2").get === VcfFloat(-0.2))
    assert(parser.parseAll(parser.vcfFloat, "123e-23").get === VcfFloat(123e-23))
    assert(parser.parseAll(parser.vcfFloat, "0.0000").get === VcfFloat(0))
  }
  
  test("vcfCharacter parses single characters") {
    assert(parser.parseAll(parser.vcfCharacter, "a").get === VcfCharacter('a'))
    assert(parser.parseAll(parser.vcfCharacter, "A").get === VcfCharacter('A'))
    assert(!parser.parseAll(parser.vcfCharacter, "ab").successful)
  }
  
  test("vcfString parses any string") {
    assert(parser.parseAll(parser.vcfString(".*".r), "abc").get === VcfString("abc"))
    assert(parser.parseAll(parser.vcfString("abc".r), "abc").get === VcfString("abc"))
    assert(!parser.parseAll(parser.vcfString("[^,]".r), "a,c").successful)
  }
  
  test("vcfFlat is always successful") {
    assert(parser.parseAll(parser.vcfFlag, "").successful)
  }
}