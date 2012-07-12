VCFImp
======

VCFImp is a Scala library for parsing and writing VCF (and VCF-like) files.

Currently, the parser is rather strict. It requires all INFO and FORMAT fields
used are previously defined in the VCF's metadata. It'll also validate that the
fields values are indeed instances of the type indicated in the metadata and
that the correct *arity* is being adhered to.

vcflatten
---------

This is a command line tool for "flattening" a VCF (4+) file down to simpler TSV
files. Essentially, it takes the information from the INFO column and from the
sample columns and spreads them out into their own, separate columns.

Check out [our documentation page on the web site](http://innovativemedicine.ca/tools/vcflatten)
for more info.

annovcf
-------

This is a command-line tool that automates the merging of ANNOVAR variation
annotation into a VCF file. It will put the annotations into the appropriate
VCF field (eg. INFO field) and create the necessary metadata at the top of the
VCF file.

Check out [our documentation page on the web site](http://innovativemedicine.ca/tools/annovcf)
for more info.

vcfimp-solr
-----------

This is a set of classes for importing a VCF file into Solr.

Building VCFImp
---------------

If you wish to compile the source yourself, you'll need to get Scala and SBT.
After, you can just build the project using sbt. If you make any changes, make
sure the tests still pass:

    $ sbt
    > test

The apps (namely vcflatten right now) have an additional SBT task "dist" that
generates a zip file distribution of the app.

    $ sbt
    > project vcflatten
    > dist

This will create a file `vcflatten/target/vcflatten-VERSION.zip`, which has any
additional scripts and documentation in it, along with a JAR file that contains
the app, Scala's library, and all other dependencies in it.

