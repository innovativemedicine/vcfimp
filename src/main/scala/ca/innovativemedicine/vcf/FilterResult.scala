package ca.innovativemedicine.vcf


sealed trait FilterResult

object FilterResult {
  case object Pass extends FilterResult
  case class Fail(reasons: List[Metadata.Filter]) extends FilterResult
}