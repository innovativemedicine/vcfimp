annovcf - A tool for annotating VCF files with ANNOVAR
======================================================

ANNOVAR provides a convenient way to annotate variants and filter them. It can
work with VCF files, but does not directly integrate the annotations into the
VCF. This tools uses ANNOVAR to obtain annotations, then mixes them back into
the INFO and FILTER fields of variants in the VCF file. So, you get the
benefits of ANNOVAR, without having to lose the VCF.

It currently supports adding several gene based annotations (using ANNOVAR's
--geneanno option), and SIFT and PolyPhen scores.

Installation
------------

This program relies on 2 external programs: *ANNOVAR* and *vcf-annovar-patch*.
The latter, *vcf-annovar-patch*, comes with *annovcf*, so you don't need to
worry about it. However, you must install *ANNOVAR* yourself and make it
available to annovcf. You must also have ANNOVAR download several DBs.

1. Get annovcf and unzip it somewhere (unzip annovcf.zip).
2. Make annovcf executable.
	
	chmod +x annovcf/bin/annovcf

2. [Get ANNOVAR](http://www.openbioinformatics.org/annovar/annovar_download.html "ANNOVAR download page")
   and install it.
3. Download the required DBs (*Note: by default, annovcf will use ANNOVAR's humandb
   sub-directory for the DBs*):

       annovar/annotate_variation.pl --downdb refGene -build hg19 annovar/humandb
       annovar/annotate_variation.pl --downdb avsift -build hg19 annovar/humandb
       annovar/annotate_variation.pl --downdb ljb_pp2 -webfrom annovar -build hg19 annovar/humandb

4. Make sure annovcf can see ANNOVAR. You have 3 options:
	- put ANNOVAR's root directory on your path (eg. PATH=$PATH:./annovar),
	- symlink ANNOVAR's root directory to annovcf/annovar (eg. ln -s ./annovar ./annovcf/annovar),
	- provide the path with the --annovar <dir> switch (eg. ./annovcf/annovcf --annovar ./annovar ...).

You will also need **Bash**, **Java** 5+ and **Perl**.

Running annovcf
---------------

The simplest run would be:

    annovcf in.vcf.gz | bgzip -c > out.vcf.gz

This would add the default set of annotations to `in.vcf.gz` (ie. --gene --sift
--pp2).

