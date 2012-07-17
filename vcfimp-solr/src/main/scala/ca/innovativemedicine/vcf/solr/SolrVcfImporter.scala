package ca.innovativemedicine.vcf.solr

import java.io.InputStream

import java.{ util => ju }

import scala.collection.JavaConverters._

import ca.innovativemedicine.vcf._
import ca.innovativemedicine.vcf.parsers._

import org.apache.solr.common.SolrInputDocument

import scala.collection.mutable

import com.typesafe.config.ConfigFactory



object SolrVcfImporter {
  object Collection {
    val Calls = Some("calls")
    val Variants = Some("variants")
  }
    
  def apply(): SolrVcfImporter = new SolrVcfImporter with ConfiguredSolrServerProvider with Configured {
    lazy val config = ConfigFactory.load()
  }
}


trait ConfiguredSolrVcfImporter extends SolrVcfImporter { self: SolrServerProvider with Configured =>
  lazy val workers: Int = if (config.hasPath("vcf.parser.workers")) {
    config.getInt("vcf.parser.workers")
  } else 0
  
  lazy val parallelConversion: Boolean = if (config.hasPath("vcf.solr.parallelConversion")) {
    config.getBoolean("vcf.parser.workers")
  } else false
  
  override def importVcf(in: InputStream): Unit = importVcf(in, workers)
}


trait SolrVcfImporter { self: SolrServerProvider =>
  import SolrVcfImporter._
  
  def importVcf(in: InputStream): Unit = importVcf(in, 0)
  
  def importVcf(in: InputStream, workers: Int) {
    val parser = VcfParser()

    parser.parse(in, skipErrors=false, workers=workers) { (vcfInfo, rows) =>
      val converter = new VcfRowConverter(vcfInfo)
      val convert = (converter.convert _).tupled
      
      withSolrServer(Collection.Variants) { vSolr =>
        withSolrServer(Collection.Calls) { cSolr =>
          rows map convert foreach { case (vdoc, sdocs) =>
            vSolr add vdoc
            sdocs foreach { cSolr add _ }
          }
        }
      }
    }
  }
}