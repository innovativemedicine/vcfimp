VCFImp
======

VCFImp is an API for parsing and writing VCF (and VCF-like) files in Scala. It
will also, hopefully, contain some useful tools for manipulating VCF files.

vcflatten
---------

The vcflatten program is a command line tool for "flattening" a VCF file down
to simpler TSV files.

vcflatten -i AA:AC:AF -g 'GT;GL' ../chr1.vcf.gz

Build Requirements
------------------

You will need SBT 0.11.2 to build this, due to the one-jar SBT plugin. Hopefully one-jar will be released for 0.11.3 soon and we can keep on using 0.11.3 again.

