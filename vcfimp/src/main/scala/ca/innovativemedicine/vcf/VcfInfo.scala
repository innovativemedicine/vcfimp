package ca.innovativemedicine.vcf

/**
 * VcfInfo is a holder for all of the information up to the point where the
 * actual variants start. That is, it contains all of the metadata and sample
 * names.
 */
case class VcfInfo(metadata: List[Metadata], samples: List[Sample]) {
  import Metadata._
  
  protected lazy val sampleLookup: Map[VcfId, Sample] =
    samples.map(s => s.id -> s)(collection.breakOut)
  
  /** Holds only INFO metadata */
  protected lazy val infoLookup: Map[VcfId, Info] = metadata.collect({
    case md: Info => md.id -> md
  })(collection.breakOut)
  
  /** Holds only FILTER metadata */
  protected lazy val filterLookup: Map[VcfId, Filter] = metadata.collect({
    case md: Filter => md.id -> md
  })(collection.breakOut)
  
  /** Holds only ALT metadata */
  protected lazy val altLookup: Map[VcfId, Alt] = metadata.collect({
    case md: Alt => md.id -> md
  })(collection.breakOut)
  
  /** Holds only FORMAT metadata */
  protected lazy val formatLookup: Map[VcfId, Format] = metadata.collect({
    case md: Format => md.id -> md
  })(collection.breakOut)
  
  /** Holds all other metadata, keeping the last in cases of ID conflicts. */
  protected lazy val metadataLookup: Map[VcfId, Metadata with HasID] = metadata.collect({
    // TODO: Filter out anything that is an Info, Filter, Alt, or Format
    // Maybe just specify all the things that aren't those?
    case md: Metadata with HasID => md.id -> md
  })(collection.breakOut)
  
  /** Returns the version string, if one exists. */
  lazy val version: Option[String] = getStringValue[Version]
  
  /** Returns the reference genome. */
  lazy val reference: Option[String] = getStringValue[Reference]
  
  
  def getStringValue[A <: StringMetadata](implicit A: Manifest[A]): Option[String] = {
    metadata.reverse collectFirst {
      case Unhandled(x) if manifest[Unhandled] <:< A => x
      case Version(x) if manifest[Version] <:< A => x
      case Reference(x) if manifest[Reference] <:< A => x
    }
  }
  
  
  /** Returns the metadata with ID `id` if it exists, `None` otherwise. */
  def getMetadata(id: VcfId): Option[Metadata with HasID] = metadataLookup get id
  
  
  /**
   * Returns the description of the metadata with ID `id` if it exists and has
   * a description, otherwise it returns `None`. Note that `None` does not
   * necessarily mean there isn't some piece of metadata with the ID `id` but
   * that wasn't given a description.
   */
  def getDescription(id: VcfId): Option[String] = getMetadata(id) flatMap {
    case md: HasDescription => md.description
    case _ => None
  }
  
  
  /** Returns the sample with ID `id` if it exists, `None` otherwise. */
  def getSample(id: VcfId): Option[Sample] = sampleLookup get id
  
  
  /**
   * Returns either the metadata or sample with ID `id`. Metadata is given
   * priority if both a sample and a piece of metadata have the same ID.
   */
  def get(id: VcfId): Option[Either[Sample, Metadata]] =
    getMetadata(id) map (Right(_)) orElse (getSample(id) map (Left(_)))
  
  
  /**
   * Given a type `A` and an ID `id`, this will return the metadata with ID `id`
   * iff it is an instance of `A`. Disambiguates duplicate IDs if `A` is one of
   * `Format`, `Filter`, `Info`, or `Alt`. Otherwise, returns the most recently
   * defined metadata with the requested id, if that metadata has the requested
   * type.
   */
  def getTypedMetadata[A <: Metadata](id: VcfId)(implicit m: Manifest[A]): Option[A] = {
    if(m <:< manifest[Format]) {
      // They want Format or a subtype
      (formatLookup get id).asInstanceOf[Option[A]]
    } else if(m <:< manifest[Filter]) {
      // They want Filter or a subtype
      (filterLookup get id).asInstanceOf[Option[A]]
    } else if(m <:< manifest[Info]) {
      // They want Info or a subtype
      (infoLookup get id).asInstanceOf[Option[A]]
    } else if (m <:< manifest[Alt]) {
      // // They want Alt or a subtype
      (altLookup get id).asInstanceOf[Option[A]]
    } else {
      // Fall back on the old method: they may have asked for a supertype or
      // some other complicated thing, so just get the last `Metadata` with the
      // given ID, if it matches the given type. TODO: Work out some sort of
      // amazing typed multi-map that can get us things indexed by string and
      // any supertype.
      getMetadata(id) flatMap { md =>
        val M = md match {
    	  case _: Format => manifest[Format]
      	  case _: Filter => manifest[Filter]
      	  case _: Info => manifest[Info]
      	  case _: Alt => manifest[Alt]
        }

        // If it's the right type, make it that type and send it back
        if (M <:< m) Some(md.asInstanceOf[A]) else None
      }
    }
  }
}
