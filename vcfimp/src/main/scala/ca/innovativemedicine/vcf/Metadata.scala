package ca.innovativemedicine.vcf


/**
 * Specifies airty information about a metadata field (ie. Number=...).
 * 
 * The actual arity can be an exact number, a variable amount (0..*), or can be
 * made to match the number of alleles or genotypes.
 */
sealed trait Arity

object Arity {
  case object MatchAlleleCount extends Arity  		// A
  case object MatchGenotypeCount extends Arity		// G
  case object Variable extends Arity				// .
  case class Exact(n: Int) extends Arity			// #
}


/** 
 * Specifies type information for a metadata field (eg. FORMAT and INFO).
 */
sealed trait Type

/** Same as `Type`, but excludes `FlagType`. */
sealed trait RestrictedType extends Type

object Type {
  case object IntegerType extends RestrictedType
  case object FloatType extends RestrictedType
  case object CharacterType extends RestrictedType
  case object StringType extends RestrictedType
  case object FlagType extends Type
}


/**
 * A piece of metadata, usually mapping 1-1 with a line in a VCF file.
 */
sealed trait Metadata

object Metadata {
  
  /** A mix-in for metadata that has a VCF ID. */
  trait HasID { self: Metadata =>
    def id: VcfId
  }
  
  /** A mix-in for metadata that may come with a description. */
  trait HasDescription { self: Metadata =>
    def description: Option[String]
  }
  
  trait HasArity { self: Metadata =>
    def arity: Arity
  } 
  
  trait HasType { self: Metadata =>
    def typed: Type
  }
  
  sealed trait StringMetadata extends Metadata {
    def value: String
  }
  
  case class Unhandled(value: String) extends StringMetadata
  
  case class Version(value: String) extends StringMetadata
  
  case class Reference(value: String) extends StringMetadata
  
  case class Filter(
      id: VcfId, 
      description: Option[String]
    ) extends Metadata with HasID with HasDescription

  case class Format(
      id: VcfId,
      arity: Arity,
      typed: RestrictedType,
      description: Option[String]
    ) extends Metadata with HasID with HasDescription with HasArity with HasType
  
  case class Info(
      id: VcfId,
      arity: Arity,
      typed: Type,
      description: Option[String]
    ) extends Metadata with HasID with HasDescription with HasArity with HasType
  
  case class Alt(
      id: VcfId,
      description: Option[String]
    ) extends Metadata with HasID with HasDescription
}
