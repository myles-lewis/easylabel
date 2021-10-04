## R CMD check results
0 errors ✓ | 0 warnings ✓ | 0 notes ✓

## Resubmission
This is a resubmission. In this version I have:

* Fixed the LICENSE file to CRAN format

* Reduced the package size to 4 MB

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