package ca.innovativemedicine.vcf

/**
 * VcfInfo is a holder for all of the information up to the point where the
 * actual variants start. That is, it contains all of the metadata and sample
 * names.
 */
trait VcfInfo {
  import Metadata._
  
  /** All the Metadata associated with a VCF file. */
  def metadata: Seq[Metadata]
  
  /** All of the samples being called in a VCF file. */
  def samples: Seq[Sample]
  
  protected lazy val sampleLookup: Map[VcfId, Sample] =
    samples.map(s => s.id -> s)(collection.breakOut)
  
  protected lazy val metadataLookup: Map[VcfId, Metadata with HasID] = metadata.collect({
    case md: Metadata with HasID => md.id -> md
  })(collection.breakOut)
  
  
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
   
  
  /** Returns a list of all the declared `Info` fields, in order. */ 
  def getInfoFields: Seq[Info] = metadata collect {
    case info: Info => info
  }
  
  
  def getFormatFields: Seq[Format] = metadata collect {
    case fmt: Format => fmt
  }
  
  
  /**
   * Given a type `A` and an ID `id`, this will return the metadata with ID
   * `id` iff it is an instance of `A`.
   */
  def getTypedMetadata[A <: Metadata](id: VcfId)(implicit A: Manifest[A]): Option[A] = {
    getMetadata(id) flatMap { md =>
      val M = md match {
    	case _: Format => manifest[Format]
    	case _: Filter => manifest[Filter]
    	case _: Info => manifest[Info]
    	case _: Alt => manifest[Alt]
      }
      
      if (M <:< A) Some(md.asInstanceOf[A]) else None
    }
  }
}


object VcfInfo {
  def apply(md: List[Metadata], ss: List[Sample]): VcfInfo = new VcfInfo {
    val metadata = md
    val samples = ss
  }
}