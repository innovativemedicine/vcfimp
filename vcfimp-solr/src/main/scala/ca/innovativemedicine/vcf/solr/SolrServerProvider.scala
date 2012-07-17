package ca.innovativemedicine.vcf.solr

import org.apache.solr.common.SolrInputDocument
import org.apache.solr.client.solrj.SolrServer
import org.apache.solr.client.solrj.impl.ConcurrentUpdateSolrServer

import scala.collection.mutable


/**
 * Provides instances of `SolrServer`s that can be used to query and update
 * Solr.
 */
trait SolrServerProvider {
    
  /**
   * Given a Solr `core`, this gives `f` a `SolrServer` for that core. If
   * `core` is `None`, then the default core is used. Note that the SolrServer
   * is only valid inside `f`.
   * 
   * TODO: It may be wise to split this up into 2 functions: one for use with
   * updates and one for reads, so different `SolrServer`s can be used.
   * However, I did check out the source, and `StreamingUpdateSolrServer` does
   * just fall back to a normal `SolrServer` for anything other than updates,
   * so I doubt there will be any gain in such a split for the time being.
   * 
   * @param core The `Solr` core to use.
   * @param f The callback function that will use a `SolrServer`.
   */
  def withSolrServer[A](core: Option[String] = None)(f: SolrServer => A): A
}


/**
 * Uses a Typesafe Config to determine the variables needed to connect to a
 * Solr server.
 */
trait ConfiguredSolrServerProvider extends SolrServerProvider { self: Configured =>
  
  // Obviously the most important, the URL of the solr instance.
  private lazy val url = if (config.hasPath("solr.url")) {
    val u = config.getString("solr.url")
    if (u.endsWith("/")) u else (u + "/")
  } else "http://localhost:8080/solr/"
  
  // This is the batch size of the updates for Solr.
  private lazy val queueSize = if (config.hasPath("solr.update.queueSize")) {
    config.getInt("solr.update.queueSize")
  } else 1000
  
  // This is the number of threads that will handle requests to Solr.
  private lazy val threads = if (config.hasPath("solr.update.threads")) {
    config.getInt("solr.update.threads")
  } else 2


  def withSolrServer[A](core: Option[String] = None)(f: SolrServer => A): A = {
    val coreUrl = core map (url + _) getOrElse url
    val solr = new ConcurrentUpdateSolrServer(coreUrl, queueSize, threads)
    val result = f(solr)
    solr.commit()
    solr.blockUntilFinished()
    result
  }
}
