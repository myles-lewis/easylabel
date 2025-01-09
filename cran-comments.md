## R CMD check results
0 errors ✓ | 0 warnings ✓ | 0 notes ✓

## Update to version 0.3.2
Added new features:
* Save and restore settings from within the shiny interface.
* Option to export millions of points as embedded raster bitmap while other
plotting elements (axes, titles, labels and lines etc) are preserved as vector
objects.

## Update
Update to version 0.2.7. In this version we have:

* Added a new plotting feature for log p-value QQ plots.

* Fixed bugs relating to use of matrices.

## Update
This is an update to version 0.2.4. In this version we have:

* Added a feature to allow the option for label text, line and box colours to 
  match colour of each point
  
* Enabled optional return value of plotly objects from easylabel()

* Fixed bugs in MA plot

## Update
This is an update to new version 0.2.2. In this version I have:

* Added a new feature to plot Manhattan plots using a new function 
  easyManhattan()
  
* Improved stability in the shiny app

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