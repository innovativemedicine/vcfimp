package ca.innovativemedicine.vcf.solr

import com.typesafe.config.Config


trait Configured {
  def config: Config
}