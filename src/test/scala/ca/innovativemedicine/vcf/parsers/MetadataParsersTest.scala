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
  
  test("whitespace and garbage is unhandled") {
    val p = parser
    assert(p.parse(p.header, "  ").get === Right(Unhandled("")))
    assert(p.parse(p.header, "").get === Right(Unhandled("")))
  }
  
  test("garbage causes parser to fail") {
    val p = parser
    assert(!p.parseAll(p.header, "lkjasdlkfj").successful)
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
    assert(p.parse(p.version, "fileformat=VCFv4.1").get === Version("VCFv4.1"))
    assert(p.parse(p.metadata, "##fileformat=VCFv4.1").get === Version("VCFv4.1"))
    assert(p.parse(p.version, "fileformat=asdfqsdf").get === Version("asdfqsdf"))
  }
  
  test("source, assembly, contig, SAMPLE and PEDIGREE are unhandled") {
    val p = parser
    assert(p.parse(p.metadata, "##source=asdf").get === Unhandled("source=asdf"))
    assert(p.parse(p.metadata, "##assembly=asdf").get === Unhandled("assembly=asdf"))
    assert(p.parse(p.metadata, "##contig=<ID=20,length=62435964,assembly=B36,md5=f126cdf8a6e0c7f379d618ff66beb2da,species=\"Homo sapiens\",taxonomy=x>").get === Unhandled("contig=<ID=20,length=62435964,assembly=B36,md5=f126cdf8a6e0c7f379d618ff66beb2da,species=\"Homo sapiens\",taxonomy=x>"))
    assert(p.parse(p.metadata, "##SAMPLE=<ID=S_ID,Genomes=G1_ID;G2_ID; ...;GK_ID,Mixture=N1;N2; ...;NK,Description=S1;S2; ...; SK >").get === Unhandled("SAMPLE=<ID=S_ID,Genomes=G1_ID;G2_ID; ...;GK_ID,Mixture=N1;N2; ...;NK,Description=S1;S2; ...; SK >"))
    assert(p.parse(p.metadata, "##PEDIGREE=<Name_0=G0-ID,Name_1=G1-ID,...,Name_N=GN-ID>").get === Unhandled("PEDIGREE=<Name_0=G0-ID,Name_1=G1-ID,...,Name_N=GN-ID>"))
  }
  
  test("valid IDs don't contain ,.<>") {
    val p = parser
    assert(p.parseAll(p.id, "ID=AA").get === VcfId("AA"))
    assert(p.parseAll(p.id, "ID=:S:ASJD:...12303#!@#").get === VcfId(":S:ASJD:...12303#!@#"))
  }
  
  test("empty IDs are invalid") {
    val p = parser
    assert(!p.parseAll(p.id, "ID=").successful)
  }
  
  test("invalid IDs have [,<>] in them") {
    val p = parser
    assert(!p.parseAll(p.id, "<").successful)
    assert(!p.parseAll(p.id, ">").successful)
    assert(!p.parseAll(p.id, ",").successful)
  }
  
  test("arity can be 'A', 'G', '.', or a non-negative integer") {
    val p = parser
    assert(p.parseAll(p.arity, "Number=A").get === Arity.MatchAlleleCount)
    assert(p.parseAll(p.arity, "Number=G").get === Arity.MatchGenotypeCount)
    assert(p.parseAll(p.arity, "Number=2").get === Arity.Exact(2))
    assert(p.parseAll(p.arity, "Number=0").get === Arity.Exact(0))
    assert(p.parseAll(p.arity, "Number=.").get === Arity.Variable)
    assert(!p.parseAll(p.arity, "Number=-2").successful)
    assert(!p.parseAll(p.arity, "Number=x").successful)
    assert(!p.parseAll(p.arity, "Number=").successful)
    assert(!p.parseAll(p.arity, "Number=2.3123").successful)
  }
  
  test("type can only be Integer, Float, Character, String, or Flag") {
    val p = parser
    assert(p.parseAll(p.typed, "Type=Integer").get === Type.IntegerType)
    assert(p.parseAll(p.typed, "Type=Float").get === Type.FloatType)
    assert(p.parseAll(p.typed, "Type=Character").get === Type.CharacterType)
    assert(p.parseAll(p.typed, "Type=String").get === Type.StringType)
    assert(p.parseAll(p.typed, "Type=Flag").get === Type.FlagType)
    
    assert(!p.parseAll(p.typed, "Type=").successful)
    assert(!p.parseAll(p.typed, "Type=Double").successful)
    assert(!p.parseAll(p.typed, "Type=Ajksldf").successful)
  }
  
  test("restricted type cannot be Flag") {
    val p = parser
    assert(p.parseAll(p.shortTyped, "Type=Integer").get === Type.IntegerType)
    assert(p.parseAll(p.shortTyped, "Type=Float").get === Type.FloatType)
    assert(p.parseAll(p.shortTyped, "Type=Character").get === Type.CharacterType)
    assert(p.parseAll(p.shortTyped, "Type=String").get === Type.StringType)
    
    assert(!p.parseAll(p.shortTyped, "Type=Flag").successful)
  }
  
  test("description must be quoted") {
    val p = parser
    assert(p.parseAll(p.description, "Description=\"Asdf.\"").get === "Asdf.")
    assert(!p.parseAll(p.description, "Description=Asdf.").successful)
  }
  
  test("description can contain escaped slashes and quotes") {
    val p = parser
    assert(p.parseAll(p.description, """Description="asdf\\\""""").get === """asdf\"""")
  }
}