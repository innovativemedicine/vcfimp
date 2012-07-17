package ca.innovativemedicine.vcf.solr

import java.{ util => ju }

import scala.collection.JavaConverters._

import ca.innovativemedicine.vcf._
import ca.innovativemedicine.vcf.format._
import ca.innovativemedicine.vcf.parsers._

import org.apache.solr.common.SolrInputDocument

import scala.util.MurmurHash
import scala.collection.mutable


object VcfRowConverter {
  import VcfFormatter._
  
  // TODO: This should be be an LRU map or something.
  private val cache = new mutable.HashMap[(Int, Int), List[List[Int]]] with mutable.SynchronizedMap[(Int, Int), List[List[Int]]]
  
  def orderGenotypes(ploidy: Int, alleles: Int): List[List[Int]] = {
    require(ploidy >= 1)
    require(alleles >= 1)
    
    def order(ploidy: Int, alts: Int): List[List[Int]] = if (ploidy == 1) {
      ((0 to alts) map (_ :: Nil)).toList
    } else {
      ((0 to alts) flatMap { j =>
        order(ploidy - 1, j) map (j :: _)
      }).toList
    }
    
    cache.getOrElseUpdate((ploidy, alleles), order(ploidy, alleles - 1) map (_.reverse))
  }
  
  /**
   * We generate a *unique* ID by use the reference genome and the variant's
   * chromosome, position, reference allele and alternate alleles.
   */
  def generateVariantId(reference: String, variant: Variant): String = {
    val alts = Right(variant.reference) :: variant.alternates
    
    val h1 = new MurmurHash[Variant.Alternate](29)
    val h2 = new MurmurHash[Variant.Alternate](17)
    alts foreach h1
    alts foreach h2
    
    "%s:%s:%d:%08x-%08x" format (
        reference,
        formatChromosome(variant.chromosome),
        variant.position,
        h1.hash,
        h2.hash)
  }
}



/**
 * A VcfRowConverter can be used to convert a single row of a VCF file to
 * docs that can be indexed in Solr.
 */
case class VcfRowConverter(vcfInfo: VcfInfo) {
  private val reference = vcfInfo.reference getOrElse {
    throw new IllegalStateException("Missing required ##reference in VCF.")
  }
  
  
  def addTypedField(doc: SolrInputDocument, prefix: String, postfix: String, tpe: Type, values: List[VcfValue]) = tpe match {
    case Type.IntegerType =>
      doc.addField(prefix + "_i_" + postfix, (values collect { case VcfInteger(x) => x }).asJava)
      
    case Type.FloatType =>
      doc.addField(prefix + "_f_" + postfix, (values collect { case VcfFloat(x) => x }).asJava)
      
    case Type.CharacterType =>
      doc.addField(prefix + "_s_" + postfix, (values collect { case VcfCharacter(x) => x.toString }).asJava)
      
    case Type.StringType =>
      doc.addField(prefix + "_s_" + postfix, (values collect { case VcfString(x) => x }).asJava)
      
    case Type.FlagType =>
      doc.addField(prefix + "_b_" + postfix, (values collect { case VcfFlag => true }).asJava)
  }
  
  
  def convert(variant: Variant, formats: List[Metadata.Format], gtData: List[List[List[VcfValue]]]): (SolrInputDocument, List[SolrInputDocument]) = {
    val vdoc = new SolrInputDocument()
    
    val variantId = VcfRowConverter.generateVariantId(reference, variant)
      
    val chrom = VcfFormatter.formatChromosome(variant.chromosome)
    val alts = variant.alternates map VcfFormatter.formatAlternate
    
    vdoc.addField("id", variantId)
    
    vdoc.addField("reference", reference)
    vdoc.addField("chromosome", chrom)
    vdoc.addField("position", variant.position)
    
    vdoc.addField("ids", variant.ids.asJava)
    vdoc.addField("ref", variant.reference)
    vdoc.addField("alts", alts.asJava)
    variant.quality foreach (vdoc.addField("quality", _))
    variant.filter foreach {
      case FilterResult.Pass =>
        vdoc.addField("filters_failed", new ju.LinkedList[String]())
      case FilterResult.Fail(filters) =>
        vdoc.addField("filters_failed", filters.asJava)
    }
    
    variant.info foreach { case (info, values) =>
      addTypedField(vdoc, "info", info.id.id, info.typed, values)
    }
    
    val sdocs = (vcfInfo.samples zip gtData) map { case (sample, values) =>
      val doc = new SolrInputDocument()
      
      // Each call is given an ID unique to the variant and sample ID.
      doc.addField("id", "%s[%s]" format (variantId, sample.id.id))
      
      // Location.
      
      doc.addField("reference", reference)
      doc.addField("chromosome", chrom)
      doc.addField("position", variant.position)
      
      // Sample.
      
      doc.addField("sample", sample.id.id)
      
      // Variant.
      
      doc.addField("variant", variantId)
      doc.addField("v_ids", variant.ids.asJava)
      doc.addField("v_ref", variant.reference)
      doc.addField("v_alt", alts.asJava)
      variant.quality foreach (doc.addField("v_quality", _))
      variant.filter foreach {
        case FilterResult.Pass =>
          doc.addField("v_filters_failed", new ju.LinkedList[String]())
        case FilterResult.Fail(filters) =>
          doc.addField("v_filters_failed", filters.asJava)
      }
      
      // Call.
      
      val data = ((formats map (_.id.id)) zip values).toMap
      val ploidy = (data get "GT") collect {
        case VcfString(gt) :: Nil =>
          val unphased = (gt split "[|/]") map (_.toInt)
          doc.addField("s_gt_unphased", unphased.toList.asJava)
          doc.addField("s_gt", gt)
          unphased.size
      } getOrElse 2
      
      (data get "GL") foreach { uvals =>
        val likelihoods = uvals collect { case VcfFloat(value) => value }
        val order = VcfRowConverter.orderGenotypes(ploidy, variant.alternates.size + 1) map (_ mkString "_")
        
        (order zip likelihoods) foreach { case (ord, gtl) =>
          doc.addField("s_gl_" + ord, gtl)
        }
      }
      
      variant.info foreach { case (info, values) =>
        addTypedField(doc, "v_info", info.id.id, info.typed, values)
      }
      
      (formats zip values) foreach { case (fmt, values) =>
        addTypedField(doc, "data", fmt.id.id, fmt.typed, values)
      }
      
      doc
    }
    
    (vdoc, sdocs)
  }
}