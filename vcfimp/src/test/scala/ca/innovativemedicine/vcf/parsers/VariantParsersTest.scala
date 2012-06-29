package ca.innovativemedicine.vcf.parsers

import ca.innovativemedicine.vcf._
import org.scalatest.FunSuite

class VariantParsersTest extends FunSuite {
  import Metadata._
  
  val q10 = Filter(VcfId("q10"), None)
  
  val s50 = Filter(VcfId("s50"), None)
  
  val infos = List(
      Info(VcfId("LDAF"), Arity.Exact(1), Type.FloatType, None),
      Info(VcfId("CIEND"), Arity.Exact(2), Type.IntegerType, None),
      Info(VcfId("HOMLEN"), Arity.Variable, Type.IntegerType, None),
      Info(VcfId("AA"), Arity.Exact(1), Type.StringType, None),
      Info(VcfId("AC"), Arity.Variable, Type.IntegerType, None),
      Info(VcfId("SVTYPE"), Arity.Exact(1), Type.StringType, None),
      Alt(VcfId("DEL"), None),
      q10,
      s50
    )
      
  val vcf = VcfInfo(infos, Nil)
  
  lazy val parser = new VariantParsers {
    val vcfInfo = vcf
  }
  
  test("VCF IDs must be surrounded in angle brackets") {
    assert(parser.parseAll(parser.vcfId, "<HI>").get === VcfId("HI"))
    assert(!parser.parseAll(parser.vcfId, "HI").successful)
  }
  
  test("sequences are case insensitive strings of nucleotides (ATCG + N)") {
    assert(parser.parseAll(parser.sequence, "ATCCCA").get === "ATCCCA")
    assert(parser.parseAll(parser.sequence, "nnatcca").get === "NNATCCA")
    assert(parser.parseAll(parser.sequence, "aAtTcCgGnN").get === "AATTCCGGNN")
    
    assert(!parser.parseAll(parser.sequence, "abc").successful)
  }
  
  test("sequences cannot be empty") {
    assert(!parser.parseAll(parser.sequence, "").successful)
  }
  
  test("chromosomes can be VCF IDs") {
    assert(parser.parseAll(parser.chromosome, "<SOMEID>").get === Left(VcfId("SOMEID")))
  }
  
  test("chromosomes can be plain strings") {
    assert(parser.parseAll(parser.chromosome, "X").get === Right("X"))
  }
  
  test("positions are non-negative integers") {
    assert(parser.parseAll(parser.position, "0").get === 0)
    assert(parser.parseAll(parser.position, "1234").get === 1234)
    assert(parser.parseAll(parser.position, "2000000000").get === 2000000000)
    assert(!parser.parseAll(parser.position, "-1234").successful)
  }
  
  test("filters can be a PASS") {
    assert(parser.parseAll(parser.filters, "PASS").get === FilterResult.Pass)
  }
  
  test("filters failure must list valid (defined) reasons") {
    assert(parser.parseAll(parser.filters, "q10;s50").get === FilterResult.Fail(List(q10, s50)))
    assert(!parser.parseAll(parser.filters, "abc").successful)
  }
  
  object Breakends {
    val seq = "ATCG"
    val chr = Right("13")
    val pos = 1234
    
    val case1 = "atcg[13:1234["
    val case2 = "atcg]13:1234]"
    val case3 = "]13:1234]atcg"
    val case4 = "[13:1234[atcg"
      
    val case1exp = Breakend(seq, chr, pos, JoinType.JoinAfter)
    val case2exp = Breakend(seq, chr, pos, JoinType.JoinReverseAfter)
    val case3exp = Breakend(seq, chr, pos, JoinType.JoinBefore)
    val case4exp = Breakend(seq, chr, pos, JoinType.JoinReverseBefore)
  }
  
  test("valid breakends can be parsed correctly") {
    import Breakends._
    
    assert(parser.parseAll(parser.breakend, case1).get === case1exp)
    assert(parser.parseAll(parser.breakend, case2).get === case2exp)
    assert(parser.parseAll(parser.breakend, case3).get === case3exp)
    assert(parser.parseAll(parser.breakend, case4).get === case4exp)
  }
  
  test("breakends must have a location and non-empty sequence") {
    assert(!parser.parseAll(parser.breakend, "").successful)
    assert(!parser.parseAll(parser.breakend, "[[").successful)
    assert(!parser.parseAll(parser.breakend, "]]").successful)
    assert(!parser.parseAll(parser.breakend, "atcg[[").successful)
    assert(!parser.parseAll(parser.breakend, "[[atcg").successful)
    assert(!parser.parseAll(parser.breakend, "[13:1234[").successful)
  }
  
  test("there must be at least 1 alternate") {
    assert(!parser.parseAll(parser.alternates, "").successful)
  }
  
  test("alternate can be a VCF ID") {
    assert(parser.parseAll(parser.alternates, "<ALTID>").get === Left(Right(VcfId("ALTID"))) :: Nil)
  }
  
  
  test("alternate can be a sequence") {
    assert(parser.parseAll(parser.alternates, "atcGN").get === Right("ATCGN") :: Nil)
  }
  
  test("alternate can be a breakend") {
    import Breakends._
    assert(parser.parseAll(parser.alternates, case1).get === Left(Left(case1exp)) :: Nil)
    assert(parser.parseAll(parser.alternates, case2).get === Left(Left(case2exp)) :: Nil)
    assert(parser.parseAll(parser.alternates, case3).get === Left(Left(case3exp)) :: Nil)
    assert(parser.parseAll(parser.alternates, case4).get === Left(Left(case4exp)) :: Nil)
  }
  
  test("multiple alternates are allowed") {
    import Breakends._
    assert(parser.parseAll(parser.alternates, case1 + ",atcg,<ALTID>,nnn," + case3).get ===
      Left(Left(case1exp)) :: Right("ATCG") :: Left(Right(VcfId("ALTID"))) :: Right("NNN") :: Left(Left(case3exp)) :: Nil)
  }
  
  test("can parse full, valid variant data") {
    val row = "22\t16050408\trs149201999\tT\tC\t100\tPASS\tLDAF=0.0649;AA=.;AC=134"
    assert(parser.parseAll(parser.variant, row).successful)
  }
  
  test("ID, QUAL and FILTERS are optional fields") {
    val missing = "22\t16050408\t.\tT\tC\t.\t.\t"
    assert(parser.parseAll(parser.variant, missing).successful)
  }
  
  test("variant row requires CHROM, POS, ID, REF, and ALT") {
    val a = "\t16050408\trs149201999\tT\tC\t.\t.\t"
    val b = "22\t\trs149201999\tT\tC\t.\t.\t"
    val d = "22\t16050408\trs149201999\t\tC\t.\t.\t"
    val e = "22\t16050408\trs149201999\tT\t\t.\t.\t"
      
    assert(!parser.parseAll(parser.variant, a).successful)
    assert(!parser.parseAll(parser.variant, b).successful)
    assert(!parser.parseAll(parser.variant, d).successful)
    assert(!parser.parseAll(parser.variant, e).successful)
  }
}