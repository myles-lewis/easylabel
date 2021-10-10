## R CMD check results
0 errors ✓ | 0 warnings ✓ | 0 notes ✓

## Resubmission
This is a resubmission. In this version I have:

* Fixed the description to refer to 'shiny' and 'plotly' in single quotes and 
  used lower case for 'shiny' and 'plotly'.

* \value{No return value} has been added to:
  easylabel.Rd
  easyMAplot.Rd
  easyVolcano.Rd

* \dontrun{} has been removed from the examples. The example is a 'shiny' app 
  and user interactive, and therefore never completes unless there is user 
  intervention. But it is enclosed in if(interactive()) { ... }

* Lines 531 and 532 of easylabel.R have been changed to the following, before 
  the call to change par() which occurs in line 533:
  oldpar <- par(no.readonly = TRUE)  # line 531
  on.exit(par(oldpar))  # line 532
  par( ... )  # line 533

## RHub check_for_cran results
2 NOTES

* checking package dependencies ... NOTE
Packages suggested but not available for checking:
  'AnnotationDbi', 'org.Hs.eg.db', 'qvalue'
* checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: 'AnnotationDbi'

I have included requireNamespace() checks for all 3 Bioconductor packages with 
warning messages giving instructions on installation from Bioconductor.

## Downstream dependencies
There are currently no downstream dependencies for this package