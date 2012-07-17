package ca.innovativemedicine.vcf.app

import ca.innovativemedicine.vcf.solr._

import java.io.{ File, InputStream, BufferedInputStream, FileInputStream }
import com.typesafe.config.{ Config, ConfigException, ConfigFactory }


case class SolrVcfImportOpts(config: Config, in: Option[InputStream])


object SolrVcfImport extends App {

  parseOpts(SolrVcfImportOpts(ConfigFactory.load(), None), args.toList) match {
    case Left(error) =>
      println(error)
      
    case Right(opts) =>
      val importer = new ConfiguredSolrVcfImporter with ConfiguredSolrServerProvider with Configured {
        val config = opts.config
      }
      
      println("Importing VCF to Solr.")
      importer.importVcf(opts.in getOrElse System.in)
      println("Finished!")
  }
  
  
  def parseOpts(opts: SolrVcfImportOpts, args: List[String]): Either[String, SolrVcfImportOpts] = args match {
    case Nil =>
      Right(opts)

    case ("-c" | "--config") :: configFilename :: args =>
      try {
        val f = new java.io.File(configFilename)
        if (!f.canRead()) {
          Left("Cannot read config file: %s" format configFilename)
        } else {
          val config = ConfigFactory.parseFile(f)
          parseOpts(opts.copy(config = config.withFallback(opts.config)), args)
        }
        
      } catch {
        case e: ConfigException =>
          Left("Failed to parse config: " + e.getMessage())
      }
      
    case vcfFilename :: args =>
      val f = new java.io.File(vcfFilename)
      if (!f.canRead()) {
        Left("Cannot read VCF file: %s" format vcfFilename)
      } else {
        val in = new BufferedInputStream(new FileInputStream(f))
        parseOpts(opts.copy(in = Some(in)), args)
      }
  }
}