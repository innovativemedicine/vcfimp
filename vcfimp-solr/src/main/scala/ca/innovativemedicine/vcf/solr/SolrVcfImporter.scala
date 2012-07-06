package ca.innovativemedicine.vcf.solr

import java.io.File

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


trait SolrVcfImporter { self: SolrServerProvider =>
  import SolrVcfImporter._
      
  def importFile(file: File, parallel: Boolean = false) {
    val parser = VcfParser()
    
    parser.parse(file, skipErrors=false) { (vcfInfo, rows) =>
      val converter = new VcfRowConverter(vcfInfo)
      
      withSolrServer(Collection.Variants) { vSolr =>
        withSolrServer(Collection.Calls) { cSolr =>
          rows map ((converter.convert _).tupled) foreach {
            case (vdoc, sdocs) =>
              vSolr add vdoc
              sdocs foreach { cSolr add _ }
          }
        }
      }
    }
  }
}