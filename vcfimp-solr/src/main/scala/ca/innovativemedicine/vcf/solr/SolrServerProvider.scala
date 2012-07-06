package ca.innovativemedicine.vcf.solr

import org.apache.solr.common.SolrInputDocument
import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.impl.StreamingUpdateSolrServer

import scala.collection.mutable


trait SolrServerProvider {
  def withSolrServer[A](core: Option[String] = None)(f: SolrServer => A): A
}

trait ConfiguredSolrServerProvider extends SolrServerProvider { self: Configured =>
  
  private lazy val url = if (config.hasPath("solr.url")) {
    val u = config.getString("solr.url")
    if (u.endsWith("/")) u else (u + "/")
  } else "http://localhost:8080/solr/"
    
  private lazy val queueSize = if (config.hasPath("solr.update.queueSize")) {
    config.getInt("solr.update.queueSize")
  } else 1000
  
  private lazy val threads = if (config.hasPath("solr.update.threads")) {
    config.getInt("solr.update.threads")
  } else 2

  
  def withSolrServer[A](core: Option[String] = None)(f: SolrServer => A): A = {
    val coreUrl = core map (url + _) getOrElse url
    val solr = new StreamingUpdateSolrServer(coreUrl, queueSize, threads)
    val result = f(solr)
    solr.commit()
    result
  }
}

