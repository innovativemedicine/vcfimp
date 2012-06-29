package ca.innovativemedicine.vcf


sealed trait JoinType
object JoinType {
  case object JoinAfter extends JoinType
  case object JoinBefore extends JoinType
  case object JoinReverseAfter extends JoinType
  case object JoinReverseBefore extends JoinType
}

case class Breakend(alt: String, chromosome: Either[VcfId, String], position: Int, joinType: JoinType) {
  import JoinType._
  
  private def location = chromosome.fold(_.toString, identity) + ":" + position
  
  def toBreakendString: String = joinType match {
    case JoinAfter => alt + "[" + location + "["
    case JoinBefore => "]" + location + "]" + alt
    case JoinReverseAfter => alt + "]" + location + "]"
    case JoinReverseBefore => "[" + location + "[" + alt
  }
}
