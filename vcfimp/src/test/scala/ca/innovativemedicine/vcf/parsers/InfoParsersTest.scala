package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._
import org.scalatest.FunSuite


class InfoParsersTest extends FunSuite {
  import Metadata._
  
  val f2 = Info(VcfId("F2"), Arity.Exact(2), Type.FloatType, None)
  val iv = Info(VcfId("IV"), Arity.Variable, Type.IntegerType, None)
  val s1 = Info(VcfId("S1"), Arity.Exact(1), Type.StringType, None)
  val ca = Info(VcfId("CA"), Arity.MatchAlleleCount, Type.CharacterType, None)
  val ig = Info(VcfId("IG"), Arity.MatchGenotypeCount, Type.IntegerType, None)
  val b = Info(VcfId("B"), Arity.Exact(0), Type.FlagType, None)
  
  val infos = List(f2, iv, s1, ca, ig, b)
  
  lazy val parser = new InfoParsers {
    val vcfInfo = VcfInfo(infos, Sample(VcfId("a")) :: Sample(VcfId("b")) :: Nil)
  }
  
  test("valid INFO is parsed correctly") {
    val info = "F2=1,2.3;IV=1,2,3,4;S1=asdf;CA=a,b,c;B"
    assert(parser.parseAll(parser.info(3), info).get ===
      Map(f2 -> List(VcfFloat(1), VcfFloat(2.3)),
          iv -> List(VcfInteger(1), VcfInteger(2), VcfInteger(3), VcfInteger(4)),
          s1 -> List(VcfString("asdf")),
          ca -> List(VcfCharacter('a'), VcfCharacter('b'), VcfCharacter('c')),
          b  -> List(VcfFlag)))
  }
  
  test("missing-data INFO is parsed correctly") {
    val info = "."
    assert(parser.parseAll(parser.info(3), info).get === Map())
  }
  
  test("partially-full INFOs should parse") {
    assert(parser.parseAll(parser.info(3), "F2=1.0,2").successful)
    assert(parser.parseAll(parser.info(3), "F2=1.0,2;IV=").successful)
  }
  
  test("mismatched arities should fail") {
    assert(!parser.parseAll(parser.infoField(2), "F2=1.0").successful)
    assert(!parser.parseAll(parser.infoField(2), "S1=asdf,fdsa").successful)
    assert(!parser.parseAll(parser.infoField(2), "CA=a,b,c").successful)
    assert(!parser.parseAll(parser.infoField(2), "B=0").successful)
  }
  
  test("cannot use MatchGenotypeCount in INFO") {
    assert(!parser.parseAll(parser.infoField(2), "IG=1").successful)
  }
}
