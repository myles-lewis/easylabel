## R CMD check results
0 errors ✓ | 0 warnings ✓ | 0 notes ✓

## This is a new submission

## RHub check_for_cran results
1 ERROR due to missing Bioconductor packages:

* Error : Bioconductor does not yet build and check packages for R version 4.2; see
     https://bioconductor.org/install
     
## RHub check_for_cran results with _R_CHECK_FORCE_SUGGESTS_ = FALSE
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