package ca.innovativemedicine.vcf.format

import ca.innovativemedicine.vcf._

import org.scalatest.FunSuite


class VcfFormatterTest extends FunSuite {
  import VcfFormatter._
  
  val ch1 = Left(VcfId("blah blah"))
  val ch2 = Right("X")
  
  val b1 = Breakend("ATCG", ch1, 1234, JoinType.JoinAfter)
  val b2 = Breakend("ATCG", ch2, 1234, JoinType.JoinReverseAfter)
  val b3 = Breakend("ATCG", ch2, 1234, JoinType.JoinBefore)
  val b4 = Breakend("ATCG", ch1, 1234, JoinType.JoinReverseBefore)
  
  val alt1 = Left(Left(b1))
  val alt2 = Left(Right(VcfId("DEL")))
  val alt3 = Right("ATCG")
  val alt4 = Right("A")
  
  val ftr1 = Metadata.Filter(VcfId("q10"), Some("Quality below 10"))
  val ftr2 = Metadata.Filter(VcfId("s50"), Some("Less than 50% of samples have data"))
  
  test("Fixed VCF header is correct") {
    assert(formatHeader === "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT")
  }
  
  test("Formats VcfValues") {
    assert(formatVcfValue(VcfInteger(-3)) === "-3")
    assert(formatVcfValue(VcfInteger(0)) === "0")
    assert(formatVcfValue(VcfInteger(1234)) === "1234")
    
    assert(formatVcfValue(VcfFloat(0.0)) === "0.0")
    assert(formatVcfValue(VcfFloat(123e100)) === "1.23E102")
    assert(formatVcfValue(VcfFloat(-0.00001)) === "-1.0E-5")
    
    assert(formatVcfValue(VcfCharacter('a')) === "a")
    assert(formatVcfValue(VcfCharacter('?')) === "?")
    
    assert(formatVcfValue(VcfString("Hello there!")) === "Hello there!")
    
    assert(formatVcfValue(VcfFlag) === "")
  }
  
  test("Breakends are formatted correclty") {
    assert(formatBreakend(b1) === "ATCG[<blah blah>:1234[")
    assert(formatBreakend(b2) === "ATCG]X:1234]")
    assert(formatBreakend(b3) === "]X:1234]ATCG")
    assert(formatBreakend(b4) === "[<blah blah>:1234[ATCG")
  }
  
  test("Formats chromosomes as VCF IDs or strings") {
    assert(formatChromosome(ch1) === "<blah blah>")
    assert(formatChromosome(ch2) === "X")
  }
  
  test("Alternates can be breakends, VCF IDs, or strings") {
    assert(formatAlternate(alt1) === "ATCG[<blah blah>:1234[")
    assert(formatAlternate(alt2) === "<DEL>")
    assert(formatAlternate(alt3) === "ATCG")
    assert(formatAlternate(alt4) === "A")
  }
  
  test("Filter results format failures correctly") {
    assert(formatFilterResult(FilterResult.Pass) === "PASS")
    assert(formatFilterResult(FilterResult.Fail(ftr1 :: ftr2 :: Nil)) === "q10;s50")
    assert(formatFilterResult(FilterResult.Fail(ftr1 :: Nil)) === "q10")
  }
  
  test("All arities are supported") {
    assert(formatArity(Arity.Exact(1)) === "1")
    assert(formatArity(Arity.Exact(0)) === "0")
    assert(formatArity(Arity.Exact(123)) === "123")
    assert(formatArity(Arity.MatchAlleleCount) === "A")
    assert(formatArity(Arity.MatchGenotypeCount) === "G")
    assert(formatArity(Arity.Variable) === ".")
  }
  
  test("VCF types just show the type") {
    assert(formatType(Type.CharacterType) === "Character")
    assert(formatType(Type.StringType) === "String")
    assert(formatType(Type.IntegerType) === "Integer")
    assert(formatType(Type.FloatType) === "Float")
    assert(formatType(Type.FlagType) === "Flag")
  }
  
  test("INFO metadata fields are formatted correctly") {
    import Metadata._
    
    val info1 = Info(VcfId("IEA"), Arity.Exact(1), Type.IntegerType, None)
    val info2 = Info(VcfId("IVS"), Arity.Variable, Type.StringType, None)
    val info3 = Info(VcfId("IAF"), Arity.MatchAlleleCount, Type.FlagType, Some("Blah."))
    
    assert(formatMetadata(info1) === "##INFO=<ID=IEA,Number=1,Type=Integer,Description=\"\">")
    assert(formatMetadata(info2) === "##INFO=<ID=IVS,Number=.,Type=String,Description=\"\">")
    assert(formatMetadata(info3) === "##INFO=<ID=IAF,Number=A,Type=Flag,Description=\"Blah.\">")
  }
  
  test("FORMAT metadata fields are formatted correctly") {
    import Metadata._
    
    val fmt1 = Format(VcfId("FEA"), Arity.Exact(1), Type.IntegerType, None)
    val fmt2 = Format(VcfId("FVS"), Arity.Variable, Type.StringType, None)
    val fmt3 = Format(VcfId("FAF"), Arity.MatchAlleleCount, Type.FloatType, Some("Blah."))
    val fmt4 = Format(VcfId("FGC"), Arity.MatchGenotypeCount, Type.CharacterType, None)
    
    assert(formatMetadata(fmt1) === "##FORMAT=<ID=FEA,Number=1,Type=Integer,Description=\"\">")
    assert(formatMetadata(fmt2) === "##FORMAT=<ID=FVS,Number=.,Type=String,Description=\"\">")
    assert(formatMetadata(fmt3) === "##FORMAT=<ID=FAF,Number=A,Type=Float,Description=\"Blah.\">")
    assert(formatMetadata(fmt4) === "##FORMAT=<ID=FGC,Number=G,Type=Character,Description=\"\">")
  }
  
  test("ALT fields may or may not have descriptions") {
    import Metadata._
    
    assert(formatMetadata(Alt(VcfId("DEL"), None)) === "##ALT=<ID=DEL,Description=\"\">")
    assert(formatMetadata(Alt(VcfId("INS"), Some("Insert"))) === "##ALT=<ID=INS,Description=\"Insert\">")
  }
  
  test("USes ##fileformat= for Version metdata") {
    assert(formatMetadata(Metadata.Version("VCFv4.1")) === "##fileformat=VCFv4.1")
  }
  
  test("Uses ##reference= for Reference metadata") {
    assert(formatMetadata(Metadata.Reference("GRCh37")) === "##reference=GRCh37")
  }
  
  test("Passes through Unhandled metadata as-is") {
    assert(formatMetadata(Metadata.Unhandled("%!@#$lkasjdflkj")) === "%!@#$lkasjdflkj")
  }
}