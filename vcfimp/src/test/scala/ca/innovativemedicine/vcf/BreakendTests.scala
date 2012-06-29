package ca.innovativemedicine.vcf

import org.scalatest.FunSuite


class BreakendTests extends FunSuite {
  test("toBreakendString returns VCF breakend string") {
    val a = Breakend("ATCG", Right("13"), 1234, JoinType.JoinAfter)
    val b = Breakend("ATCG", Right("13"), 1234, JoinType.JoinBefore)
    val c = Breakend("ATCG", Right("13"), 1234, JoinType.JoinReverseAfter)
    val d = Breakend("ATCG", Right("13"), 1234, JoinType.JoinReverseBefore)
    
    assert(a.toBreakendString === "ATCG[13:1234[")
    assert(b.toBreakendString === "]13:1234]ATCG")
    assert(c.toBreakendString === "ATCG]13:1234]")
    assert(d.toBreakendString === "[13:1234[ATCG")
    
    val e = Breakend("ATCG", Left(VcfId("AB")), 1234, JoinType.JoinAfter)
    
    assert(e.toBreakendString === "ATCG[<AB>:1234[")
  }
}