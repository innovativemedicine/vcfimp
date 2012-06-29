package ca.innovativemedicine.vcf

import org.scalatest.FunSuite

class VcfInfoTests extends FunSuite {
  import Metadata._
  
  val infoA1 = Info(VcfId("InfoA"), Arity.Exact(1), Type.StringType, None)
  val infoA2 = Info(VcfId("InfoA"), Arity.Exact(1), Type.CharacterType, None)
  
  val infoB = Info(VcfId("InfoB"), Arity.Exact(1), Type.FloatType, None)
  
  val altA = Alt(VcfId("AltA"), None)
  
  val filterA = Filter(VcfId("FilterA"), Some("Some filter."))
  
  val samples = List(Sample(VcfId("a")), Sample(VcfId("InfoB")), Sample(VcfId("c")))
  
  val vcfInfo = VcfInfo(List(infoA1, infoA2, infoB, altA, filterA), samples)
  
  test("can get a sample by ID") {
    assert(vcfInfo.getSample(VcfId("a")) === Some(samples.head))
    assert(vcfInfo.getSample(VcfId("InfoB")) === Some(samples.tail.head))
  }
  
  test("can get metadata by ID") {
    assert(vcfInfo.getMetadata(VcfId("InfoB")) === Some(infoB))
    assert(vcfInfo.getMetadata(VcfId("AltA")) === Some(altA))
    assert(vcfInfo.getMetadata(VcfId("FilterA")) === Some(filterA))
  }
  
  test("disambiguate duplicate IDs by choosing last") {
    assert(vcfInfo.getMetadata(VcfId("InfoA")) === Some(infoA2))
  }
  
  test("get typed metadata restrictes by type") {
    assert(vcfInfo.getTypedMetadata[Info](VcfId("InfoA")) === Some(infoA2))
    assert(vcfInfo.getTypedMetadata[Filter](VcfId("InfoA")) === None)
    assert(vcfInfo.getTypedMetadata[Alt](VcfId("AltA")) === Some(altA))
    assert(vcfInfo.getTypedMetadata[Info](VcfId("AltA")) === None)
  }
  
  test("can get either sample or metadata using get") {
    assert(vcfInfo.get(VcfId("a")) === Some(Left(samples.head)))
    assert(vcfInfo.get(VcfId("AltA")) === Some(Right(altA)))
  }
  
  test("metadata has precedence, by ID, if getting either") {
    assert(vcfInfo.get(VcfId("InfoB")) === Some(Right(infoB)))
  }
  
  test("get description iff metadata has it") {
    assert(vcfInfo.getDescription(VcfId("FilterA")) === Some("Some filter."))
    assert(vcfInfo.getDescription(VcfId("InfoA")) === None)
    assert(vcfInfo.getDescription(VcfId("asdf")) === None)
  }
}