VCFImp
======

VCFImp is a Scala library for parsing and writing VCF (and VCF-like) files.

Currently, the parser is rather strict. It requires all INFO and FORMAT fields
used are previously defined in the VCF's metadata. It'll also validate that the
fields values are indeed instances of the type indicated in the metadata and
that the correct *arity* is being adhered to.

vcflatten
---------

VCFImp includes a command line tool called vcflatten. This is a command line
tool for "flattening" a VCF file down to simpler TSV files. An example of its
use could be:

    $ vcflatten --info 'AA;AC;AF' --genotype GT:GL ../chr1.vcf.gz

This will create a set of new TSV files (1 for each sample), each of which has
a list of all the variant data, the information with keys AA, AC, AF and the
GT and GL genotype data. These are all in separate columns of the TSV.

Essentially, it takes the information from the INFO column and from the sample
columns and spreads them out into their own, separate columns. By default, it
will create a separate file for each sample (output TSV file names can be
customized using the `--pattern` command line switch).

You can also generate 1 big file for all samples using the `--one-file` switch.
It'll create a row for each variant and sample (so a VCF file with 100 variants
and 10 samples, will produce a TSV with 100 * 10 = 1000 rows). An additional
column will also be inserted, which indicates which sample that rows data
pertain to.

To get more information on using `vcflatten`, run:

    $ vcflatten -h

### Installing

*Warning:* This assumes you are using a unix-like environment and are
comfortable with a shell. Moreover, the vcflatten shell script in the ZIP file
requires BASH.

You can install vcflatten by either creating a distribution .zip from the
source or by downloading a released version as a .zip file.

    $ wget https://github.com/downloads/innovativemedicine/vcfimp/vcflatten-0.5.0.zip
    $ unzip vcflatten-0.5.0.zip
    $ chmod +x vcflatten-0.5.0/bin/vcflatten
    $ ./vcflatten-0.5.0/bin/vcflatten -h

If you plan on using the tool often, you should add vcflatten-0.5.0/bin to your
path.

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

